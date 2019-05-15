// module test.codegen.expressions.PartialApplication2

var first = function(x, y, z) {
  return x;
};
exports.first = first;

var apply = function(f) {
  return (f(2, 3));
};
exports.apply = apply;

var two = (apply(function(_0, _1) {
  return (first(0, _0, _1));
}));
exports.two = two;
