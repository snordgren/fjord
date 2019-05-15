// module test.codegen.expressions.PartialApplication1

var first = function(x, y) {
  return x;
};
exports.first = first;

var apply = function(f) {
  return (f(2));
};
exports.apply = apply;

var two = (apply(function(_0) {
  return (first(0, _0));
}));
exports.two = two;
