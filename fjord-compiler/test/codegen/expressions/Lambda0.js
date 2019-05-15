// module test.codegen.expressions.Lambda0

var $plus = function(x, y) {
  return x;
};
exports.$plus = $plus;

var addLambda = function(x, y) {
  return (x + y);
};
exports.addLambda = addLambda;

var partiallyApplied = function(_0) {
  return (addLambda(1, _0));
};
exports.partiallyApplied = partiallyApplied;
