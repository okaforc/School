```js
/* circle.js */
const PI = 3.14159265359; 
function area(radius) { 
	return (radius ** 2) * PI; 
} 

function circumference(radius) { 
	return 2 * radius * PI; 
}

module.exports = { 
	PI: PI, 
	area : area, 
	circumference: circumference
}; 

/* Main.js */
let circle = require('./circle.js'); 
const r = 3; 
console.log('Circle with radius %d has area: %d and circumference %d', r, circle.area(r), circle.circumference(r));
``` 
^circle-js

```js
/* circle.mjs */
const PI = 3.14159265359; 
function area(radius) { 
	return (radius ** 2) * PI; 
} 

function circumference(radius) { 
	return 2 * radius * PI; 
}

export {
	PI, area, circumference
};

/* Main.mjs */
import * as circle from "./circle.js"
const r = 3; 
console.log('Circle with radius %d has area: %d and circumference %d', r, circle.area(r), circle.circumference(r));
``` 
^circle-js-esm

