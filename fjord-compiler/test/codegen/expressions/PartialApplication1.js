// module test.codegen.expressions.PartialApplication1

export const first = (x, y) => x;

export const apply = f => (f(2));

export const two = (apply((_0) => (first(0, _0))));
