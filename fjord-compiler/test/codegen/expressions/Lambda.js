// module test.codegen.expressions.Lambda

var $plus = (x, y) => x;
exports.$plus = $plus;

var addLambda = (x, y) => (x + y);
exports.addLambda = addLambda;

var partiallyApplied = _0 => (addLambda(1, _0));
exports.partiallyApplied = partiallyApplied;
