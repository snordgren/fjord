// module test.codegen.expressions.PartialApplication2

var first = (x, y, z) => x;
exports.first = first;

var apply = f => (f(2, 3));
exports.apply = apply;

var two = (apply((_0, _1) => (first(0, _0, _1))));
exports.two = two;
