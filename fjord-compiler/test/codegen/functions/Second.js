// module test.codegen.functions.Second

var TestUtil = require("../../../test/node_modules/@fjord-lang/test-util/dist/Util.js");

var second = function(x, y) {
  return (TestUtil.rm(x, y));
};
exports.second = second;

var two = (second(1, 2));
exports.two = two;
