// module test.codegen.functions.Curry

var first = (x, y, z) => x;
exports.first = first;

var alwaysOne = (_0, _1) => (first(1, _0, _1));
exports.alwaysOne = alwaysOne;

var alwaysTwo = (x, _0) => (first(2, x, _0));
exports.alwaysTwo = alwaysTwo;

var one = (alwaysOne(2, 3));
exports.one = one;

var two = (alwaysTwo(3, 4));
exports.two = two;
