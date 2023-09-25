In a browser, a loaded HTML file can contain several independent `<script>` tags which are loaded in order into a single global scope. Tools such as Webpack, a **module bundler,** take programs developed as separate modules and pack them into a single HTML file.

## Node.js
Modularisation using the **CommonJS** style was one of the earliest methods, but has since been superseded.

### How it worked
Each JavaScript file became a module. Each of these files can explicitly decide what to export by adding it to their `modules.exports` array. The consumer module can import all of these exports using the `require` function. This works fine with Node.js, but not in the browser, so an EMCAScript standard module interface was introduced in ECMAScript in 2015 - ES Modules. New keywords were added like `export` and `import` and in Node.js, module files now have the extension `.mjs`.

