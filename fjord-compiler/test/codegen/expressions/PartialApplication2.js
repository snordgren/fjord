// module test.codegen.expressions.PartialApplication2

export const first = (x, y, z) => x;

export const apply = f => (f(2, 3));

export const two = (apply((_0, _1) => (first(0, _0, _1))));
