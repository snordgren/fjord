// module test.codegen.operators.Precedence

export const $plus = (x, y) => x;

export const $times = (x, y) => x;

export const eleven = (1 + ($times(2, 10)));
