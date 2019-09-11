// module codegen.operators.Addition

var TestUtil = require("../../../test/node_modules/@fjord-lang/test-util/dist/Util.js");

var $plus = function(x, y) {
  return (TestUtil.rm(x, y));
};
exports.$plus = $plus;

var three = ($plus(1, 2));
exports.three = three;

var seven = ($plus(($plus(1, 2)), 4));
exports.seven = seven;
