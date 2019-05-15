// module test.codegen.expressions.PartialApplication2

const first = (x, y, z) => x;
exports.first = first;

const apply = f => (f(2, 3));
exports.apply = apply;

const two = (apply((_0, _1) => (first(0, _0, _1))));
exports.two = two;
