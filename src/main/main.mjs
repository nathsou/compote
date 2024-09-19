import { readFileSync, writeFileSync } from 'fs';

const wasmBuffer = readFileSync('./target/wasm/release/build/main/main.wasm');
const sourceFile = readFileSync('./examples/binaryop.c', 'utf-8');
const outFile = './out/binaryop.s';

const { log, flush, outputChar } = (() => {
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
      const str = new TextDecoder("utf-16").decode(new Uint16Array(outputBuffer).valueOf());
      console.log(str);
      writeFileSync(outFile, str);
    }
  }

  return { log, flush, outputChar };
})();

const importObject = {
  spectest: {
    print_char: log,
  },
  host: {
    read_source_file_char: index => {
      if (index >= sourceFile.length) {
        return 0;
      }
      
      return sourceFile.charCodeAt(index);
    },
    get_source_file_length: () => sourceFile.length,
    output_char: outputChar,
  },
};

WebAssembly.instantiate(wasmBuffer, importObject).then(
  (obj) => {
    obj.instance.exports._start();
    flush();
  },
);
