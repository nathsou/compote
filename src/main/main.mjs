import { readFileSync } from 'fs';

const wasmBuffer = readFileSync('./target/wasm/release/build/main/main.wasm');
const sourceFile = readFileSync('./examples/return_2.c', 'utf-8');

const { log, flush } = (() => {
  let buffer = [];

  function flush() {
    if (buffer.length > 0) {
      console.log(new TextDecoder("utf-16").decode(new Uint16Array(buffer).valueOf()));
      buffer = [];
    }
  }

  function log(ch) {
    if (ch == '\n'.charCodeAt(0)) { flush(); }
    else if (ch == '\r'.charCodeAt(0)) { /* noop */ }
    else { buffer.push(ch); }
  }

  return { log, flush };
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
  },
};

WebAssembly.instantiate(wasmBuffer, importObject).then(
  (obj) => {
    obj.instance.exports._start();
    flush();
  },
);
