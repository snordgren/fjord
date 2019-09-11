// module test.codegen.functions.First

var TestUtil = require("../../../test/node_modules/@fjord-lang/test-util/dist/Util.js");

var first = function(x, y) {
  return (TestUtil.rm(y, x));
};
exports.first = first;

var one = (first(1, 2));
exports.one = one;
