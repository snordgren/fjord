// module test.codegen.operators.Precedence

export const $plus = (x, y) => y;

export const $times = (x, y) => y;

export const eleven = (1 + ($times(2, 10)));
