// module test.codegen.functions.Lambda

export const addLambda = (x, y) => (x + y);

export const partiallyApplied = _0 => (addLambda(1, _0));
