import { existsSync, readFileSync, writeFileSync, unlinkSync } from 'fs';
import * as path from 'path';
import * as os from 'os';
import { exec } from 'child_process';

const ASSEMBLER = 'gcc';

// --lex: Directs it to run the lexer, but stop before parsing
// --parse: Directs it to run the lexer and parser, but stop before assembly generation
// --tacky: Directs it to run the lexer, parser, and three-address code generation, but stop before assembly generation
// --codegen: Directs it to perform lexing, parsing, and assembly generation, but stop before code emission

function parseArgs(args) {
  const flags = new Set();
  const rest = [];

  for (const arg of args) {
    if (arg.startsWith('--')) {
      flags.add(arg.slice(2));
    } else if (arg.startsWith('-')) {
      flags.add(arg.slice(1));
    } else {
      rest.push(arg);
    }
  }

  if (rest.length !== 1) {
    console.error('Usage: compote [--lex | --parse | --codegen | -S] <sourceFile>');
    process.exit(1);
  }

  return { flags, rest };
}

const { flags, rest: [sourceFile] } = parseArgs(process.argv.slice(2));
const prog = readFileSync(sourceFile, 'utf8');

function assemble(assemblyCode, directory, fileName) {
  return new Promise((resolve, reject) => {
    // Create a temporary file
    const tmpDir = os.tmpdir();
    const tmpFile = path.join(tmpDir, fileName + '.s');
    const outputFile = path.join(directory, fileName);

    // Write the assembly code to the temporary file
    writeFileSync(tmpFile, assemblyCode);

    // Construct the GCC command
    const gccCommand = `${ASSEMBLER} ${tmpFile} -o ${outputFile}`;

    // Execute GCC
    exec(gccCommand, (error, _stdout, stderr) => {
      // Clean up the temporary file
      unlinkSync(tmpFile);

      if (error) {
        reject(`${ASSEMBLER} Error: ${error.message}`);
        return;
      }

      if (stderr) {
        reject(`${ASSEMBLER} stderr: ${stderr}`);
        return;
      }

      // If successful, resolve with the path to the output file
      resolve(outputFile);
    });
  });
}

const WASM_FILE = path.join(__dirname, '../target/wasm/release/build/lib/lib.wasm');

if (!existsSync(WASM_FILE)) {
  console.error(`Error: WebAssembly file ${WASM_FILE} not found`);
  process.exit(1);
}

const wasmBuffer = readFileSync(WASM_FILE);

function createImports(onOutput) {
  let printBuffer = [];
  let outputBuffer = [];

  function flush() {
    if (printBuffer.length > 0) {
      console.log(new TextDecoder("utf-16").decode(new Uint16Array(printBuffer).valueOf()));
      printBuffer = [];
    }
  }

  function log(ch) {
    if (ch == '\n'.charCodeAt(0)) { flush(); }
    else if (ch == '\r'.charCodeAt(0)) { /* noop */ }
    else { printBuffer.push(ch); }
  }

  function outputChar(ch, done) {
    outputBuffer.push(ch);

    if (done) {
      const output = new TextDecoder("utf-16").decode(new Uint16Array(outputBuffer).valueOf());
      onOutput(output);
      outputBuffer = [];
    }
  }

  return { log, flush, outputChar };
}

function parseResult(result) {
  if (result.startsWith('ok:')) {
    return { ok: true, value: result.slice(3) };
  } else if (result.startsWith('err:')) {
    return { ok: false, value: result.slice(4) };
  } else {
    throw new Error(`Unexpected result: ${result}`);
  }
}

const driverStage = {
  lex: 0,
  parse: 1,
  tacky: 2,
  codegen: 3,
};

const operatingSystem = {
  linux: 0,
  macos: 1,
};

let stage = driverStage.codegen;
let os_ = operatingSystem.linux;
let outputCallback = output => {
  const result = parseResult(output);

  if (result.ok) {
    console.log(result.value);
  } else {
    console.error(result.value);
    process.exit(1);
  }
};

if (flags.has('lex')) {
  stage = driverStage.lex;
} else if (flags.has('parse')) {
  stage = driverStage.parse;
} else if (flags.has('tacky')) {
  stage = driverStage.tacky;
} else {
  switch (os.platform()) {
    case 'darwin':
      os_ = operatingSystem.macos;
      break;
    case 'linux':
      os_ = operatingSystem.linux;
      break;
    default:
      console.error(`Unsupported operating system: ${os.platform()}`);
      process.exit(1);
  }

  outputCallback = output => {
    const asm = parseResult(output);

    if (asm.ok) {
      const fileName = path.basename(sourceFile, path.extname(sourceFile));
      const filePath = path.dirname(sourceFile);
  
      if (flags.has('S')) {
        const asmFile = path.join(filePath, fileName + '.s');
        writeFileSync(asmFile, asm.value, 'utf8');
      } else if (flags.has('codegen')) {
        console.log(asm.value);
      } else {
        assemble(asm.value, path.dirname(sourceFile), fileName)
          .catch((error) => {
            console.error(error);
            process.exit(1);
          });
      }
    } else {
      console.error(asm.value);
      process.exit(1);
    }
  };
}

const { log, flush, outputChar } = createImports(outputCallback);

const importObject = {
  spectest: {
    print_char: log,
  },
  host: {
    read_source_file_char: index => {
      if (index >= prog.length) {
        return 0;
      }

      return prog.charCodeAt(index);
    },
    get_source_file_length: () => prog.length,
    output_char: outputChar,
  },
};

WebAssembly.instantiate(wasmBuffer, importObject).then(
  (obj) => {
    if ('_start' in obj.instance.exports) {
      obj.instance.exports._start();
    }

    const { drive } = obj.instance.exports;

    drive(stage, os_);
    flush();
  },
);
