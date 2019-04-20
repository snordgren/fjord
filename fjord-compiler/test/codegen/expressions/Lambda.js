// module test.codegen.expressions.Lambda

export const $plus = (x, y) => x;

export const addLambda = (x, y) => (x + y);

export const partiallyApplied = _0 => (addLambda(1, _0));
