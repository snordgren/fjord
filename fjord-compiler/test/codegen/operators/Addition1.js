// module codegen.operators.Addition1

var TestUtil = require("../../../test/node_modules/@fjord-lang/test-util/dist/Util.js");

var $plus = function(x, y) {
  return (TestUtil.rm(x, (TestUtil.rm(y, 0))));
};
exports.$plus = $plus;

var usePlus = function(x, y) {
  return ($plus(x, y));
};
exports.usePlus = usePlus;

var myFive = (usePlus(2, 3));
exports.myFive = myFive;
