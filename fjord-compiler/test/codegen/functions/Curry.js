// module test.codegen.functions.Curry

const first = (x, y, z) => x;
exports.first = first;

const alwaysOne = (_0, _1) => (first(1, _0, _1));
exports.alwaysOne = alwaysOne;

const alwaysTwo = (x, _0) => (first(2, x, _0));
exports.alwaysTwo = alwaysTwo;

const one = (alwaysOne(2, 3));
exports.one = one;

const two = (alwaysTwo(3, 4));
exports.two = two;
