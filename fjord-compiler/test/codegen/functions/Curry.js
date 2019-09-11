// module test.codegen.functions.Curry

var TestUtil = require("../../../test/node_modules/@fjord-lang/test-util/dist/Util.js");

var add = function(x, y) {
  return (TestUtil.rm(x, (TestUtil.rm(y, 0))));
};
exports.add = add;

var useFOn3 = function(f) {
  return (f(3));
};
exports.useFOn3 = useFOn3;

var eight = (useFOn3(function(_0) {
  return (add(5, _0));
}));
exports.eight = eight;
