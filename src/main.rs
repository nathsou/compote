use std::fs;
use std::io;
use std::process::Command;
use std::path::Path;
use std::env;
use clap::{Arg, App};
use wasmtime::*;

const CC: &str = "clang";

// Enums for driver stages and operating systems
#[derive(PartialEq)]
enum DriverStage {
    Lex,
    Parse,
    Validate,
    Tacky,
    Codegen,
}

enum OperatingSystem {
    Linux,
    Macos,
}

// Struct to hold the host state for WebAssembly
struct HostState {
    preprocessed: Vec<u16>,
    print_buffer: Vec<u16>,
    output_buffer: Vec<u16>,
    module_outputs: Vec<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let matches = App::new("compote")
        .version("0.1")
        .author("Nathan Soufflet")
        .about("A toy C Compiler")
        .arg(Arg::with_name("lex")
             .long("lex")
             .help("Run lexer only"))
        .arg(Arg::with_name("parse")
             .long("parse")
             .help("Run parser only"))
        .arg(Arg::with_name("validate")
             .long("validate")
             .help("Run validation"))
        .arg(Arg::with_name("tacky")
             .long("tacky")
             .help("Run up to three-address code generation"))
        .arg(Arg::with_name("codegen")
             .long("codegen")
             .help("Run code generation"))
        .arg(Arg::with_name("S")
             .short("S")
             .help("Compile only; do not assemble or link"))
        .arg(Arg::with_name("source")
             .required(true)
             .help("Input source file"))
        .get_matches();

    let source_file = matches.value_of("source").unwrap();

    // Determine the compilation stage
    let stage = if matches.is_present("lex") {
        DriverStage::Lex
    } else if matches.is_present("parse") {
        DriverStage::Parse
    } else if matches.is_present("validate") {
        DriverStage::Validate
    } else if matches.is_present("tacky") {
        DriverStage::Tacky
    } else {
        DriverStage::Codegen
    };

    // Determine the operating system
    let os = match env::consts::OS {
        "linux" => OperatingSystem::Linux,
        "macos" => OperatingSystem::Macos,
        other => {
            eprintln!("Unsupported operating system: {}", other);
            std::process::exit(1);
        }
    };

    // Read and preprocess the source file
    let preprocessed = preprocess(source_file)?;

    // Set up the host state for the WebAssembly module
    let host_state = HostState {
        preprocessed: preprocessed.encode_utf16().collect(),
        print_buffer: Vec::new(),
        output_buffer: Vec::new(),
        module_outputs: Vec::new(),
    };

    // Determine the path to the WebAssembly module
    let mut exe_path = env::current_exe()?;
    exe_path.pop();
    exe_path.pop();
    let wasm_file_path = exe_path.join("wasm/release/build/driver/driver.wasm");

    if !wasm_file_path.exists() {
        eprintln!("Error: WebAssembly file {} not found", wasm_file_path.display());
        std::process::exit(1);
    }

    // Set up Wasmtime engine and store
    let engine = Engine::default();
    let mut store = Store::new(&engine, host_state);

    // Compile the module
    let module = Module::from_file(&engine, &wasm_file_path)?;

    // Set up the linker and define host functions
    let mut linker = Linker::new(&engine);

    // Define the 'print_char' function
    linker.func_wrap("spectest", "print_char", |mut caller: Caller<'_, HostState>, ch: i32| {
        let host_state = caller.data_mut();
        if ch == '\n' as i32 {
            // Flush
            let decoded = String::from_utf16_lossy(&host_state.print_buffer);
            println!("{}", decoded);
            host_state.print_buffer.clear();
        } else if ch != '\r' as i32 {
            host_state.print_buffer.push(ch as u16);
        }
        Ok(())
    })?;

    // Define 'read_source_file_char' function
    linker.func_wrap("host", "read_source_file_char", |mut caller: Caller<'_, HostState>, index: i32| {
        let host_state = caller.data_mut();
        host_state.preprocessed[index as usize] as i32
    })?;

    // Define 'get_source_file_length' function
    linker.func_wrap("host", "get_source_file_length", |caller: Caller<'_, HostState>| {
        caller.data().preprocessed.len() as i32
    })?;

    // Define 'output_char' function
    linker.func_wrap("host", "output_char", |mut caller: Caller<'_, HostState>, ch: i32, done: i32| {
        let host_state = caller.data_mut();
        host_state.output_buffer.push(ch as u16);
        if done != 0 {
            let output = String::from_utf16_lossy(&host_state.output_buffer);
            host_state.module_outputs.push(output);
            host_state.output_buffer.clear();
        }
        Ok(())
    })?;

    // Instantiate the module
    let instance = linker.instantiate(&mut store, &module)?;

    // Call the '_start' function if it exists
    if let Some(_start) = instance.get_func(&mut store, "_start") {
        _start.call(&mut store, &[], &mut [])?;
    }

    // Get the 'drive' function from the module
    let drive = instance.get_func(&mut store, "drive").expect("`drive` function not found");

    // Map the driver stage and operating system to integers
    let stage_param = match stage {
        DriverStage::Lex => 0,
        DriverStage::Parse => 1,
        DriverStage::Validate => 2,
        DriverStage::Tacky => 3,
        DriverStage::Codegen => 4,
    };

    let os_param = match os {
        OperatingSystem::Linux => 0,
        OperatingSystem::Macos => 1,
    };

    // Call the 'drive' function with the appropriate parameters
    drive.call(&mut store, &[Val::I32(stage_param), Val::I32(os_param)], &mut [])?;

    // After 'drive' completes, process the outputs
    {
        let host_state = store.data();
        // Flush the print buffer
        if !host_state.print_buffer.is_empty() {
            let decoded = String::from_utf16_lossy(&host_state.print_buffer);
            println!("{}", decoded);
        }

        for output in &host_state.module_outputs {
            match parse_result(output) {
                Ok(asm_code) => {
                    // Handle the assembly code based on the flags
                    if matches.is_present("S") {
                        // Write assembly code to file
                        let file_path = Path::new(source_file);
                        let asm_file_path = file_path.with_extension("s");
                        fs::write(&asm_file_path, asm_code)?;
                    } else if matches.is_present("codegen") || stage != DriverStage::Codegen {
                        // Print assembly code
                        println!("{}", asm_code);
                    } else {
                        // Assemble the code into an executable
                        let file_path = Path::new(source_file);
                        let output_file_path = file_path.with_extension("");
                        let output_file_path = output_file_path.to_string_lossy();
                        assemble(&asm_code, &output_file_path)?;
                    }
                },
                Err(err_msg) => {
                    eprintln!("{}", err_msg);
                    std::process::exit(1);
                },
            }
        }
    }

    Ok(())
}

// Function to preprocess the source file using clang
fn preprocess(source_file: &str) -> io::Result<String> {
    let output = Command::new(CC)
        .arg("-E")
        .arg("-P")
        .arg(source_file)
        .output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(io::Error::new(io::ErrorKind::Other, format!("{} error: {}", CC, stderr)))
    } else {
        let preprocessed = String::from_utf8_lossy(&output.stdout).into_owned();
        Ok(preprocessed)
    }
}

// Function to assemble the assembly code using clang
fn assemble(assembly_code: &str, output_file_path: &str) -> io::Result<()> {
    // Create a temporary file with the assembly code
    use std::fs::File;
    use std::io::Write;
    use std::env::temp_dir;

    let temp_dir = temp_dir();
    let file_name = Path::new(output_file_path).file_stem().unwrap().to_string_lossy();
    let asm_file_path = temp_dir.join(format!("{file_name}.s"));

    {
        let mut asm_file = File::create(&asm_file_path)?;
        asm_file.write_all(assembly_code.as_bytes())?;
    }

    // Invoke clang to assemble and link
    let output = Command::new(CC)
        .arg(&asm_file_path)
        .arg("-o")
        .arg(output_file_path)
        .output()?;

    // Clean up temporary file
    fs::remove_file(&asm_file_path)?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(io::Error::new(io::ErrorKind::Other, format!("{} error: {}", CC, stderr)))
    } else {
        Ok(())
    }
}

// Function to parse the result from the WebAssembly module
fn parse_result(result: &str) -> Result<String, String> {
    if result.starts_with("ok:") {
        Ok(result[3..].to_string())
    } else if result.starts_with("err:") {
        Err(result[4..].to_string())
    } else {
        Err(format!("Unexpected result: {}", result))
    }
}
