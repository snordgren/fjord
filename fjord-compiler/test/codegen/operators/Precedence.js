// module test.codegen.operators.Precedence

var TestUtil = require("../../../test/node_modules/@fjord-lang/test-util/dist/Util.js");

var $plus = function(x, y) {
  return (TestUtil.rm(x, y));
};
exports.$plus = $plus;

var $times = function(x, y) {
  return (TestUtil.rm(x, y));
};
exports.$times = $times;

var eleven = ($plus(1, ($times(2, 5))));
exports.eleven = eleven;
