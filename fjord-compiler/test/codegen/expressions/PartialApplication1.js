// module test.codegen.expressions.PartialApplication1

var first = (x, y) => x;
exports.first = first;

var apply = f => (f(2));
exports.apply = apply;

var two = (apply((_0) => (first(0, _0))));
exports.two = two;
