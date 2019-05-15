// module test.codegen.functions.Curry

var first = function(x, y, z) {
  return x;
};
exports.first = first;

var alwaysOne = function(_0, _1) {
  return (first(1, _0, _1));
};
exports.alwaysOne = alwaysOne;

var alwaysTwo = function(x, _0) {
  return (first(2, x, _0));
};
exports.alwaysTwo = alwaysTwo;

var one = (alwaysOne(2, 3));
exports.one = one;

var two = (alwaysTwo(3, 4));
exports.two = two;
