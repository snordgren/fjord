// module codegen.Imports

var typedef_TypeDef = require("../../test/typedef/TypeDef.js");
var WebGL = require("../../test/node_modules/@fjord-lang/webgl/dist/WebGL.js");

var position = (typedef_TypeDef.Point((typedef_TypeDef.five(Object.freeze([]))), (WebGL.originY(Object.freeze([])))));
exports.position = position;

var seven = (WebGL.$plus$minus$plus(3, 4));
exports.seven = seven;
