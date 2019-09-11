// module codegen.operators.Concatenation

var TestUtil = require("../../../test/node_modules/@fjord-lang/test-util/dist/Util.js");

var $plus$plus = function(x, y) {
  return (TestUtil.rm(x, y));
};
exports.$plus$plus = $plus$plus;

var helloWorld = ($plus$plus("Hello, ", "world!"));
exports.helloWorld = helloWorld;
