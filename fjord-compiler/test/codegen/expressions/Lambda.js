// module test.codegen.expressions.Lambda

const $plus = (x, y) => x;
exports.$plus = $plus;

const addLambda = (x, y) => (x + y);
exports.addLambda = addLambda;

const partiallyApplied = _0 => (addLambda(1, _0));
exports.partiallyApplied = partiallyApplied;
