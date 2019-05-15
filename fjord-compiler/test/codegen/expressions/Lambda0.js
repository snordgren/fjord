// module test.codegen.expressions.Lambda0

var first = function(x, y) {
  return x;
};
exports.first = first;

var addLambda = function(x, y) {
  return (first(x, y));
};
exports.addLambda = addLambda;

var partiallyApplied = function(_0) {
  return (addLambda(1, _0));
};
exports.partiallyApplied = partiallyApplied;
