// module codegen.Imports

var typedef_TypeDef = require("../typedef/TypeDef.js");
var WebGL = require("../node_modules/@fjord-lang/webgl/dist/WebGL.js");

const position = (typedef_TypeDef.Point(typedef_TypeDef.five, WebGL.originY));
exports.position = position;
