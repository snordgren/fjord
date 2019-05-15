// module test.codegen.expressions.PartialApplication1

const first = (x, y) => x;
exports.first = first;

const apply = f => (f(2));
exports.apply = apply;

const two = (apply((_0) => (first(0, _0))));
exports.two = two;
