// module test.codegen.operators.Precedence

const $plus = (x, y) => y;
exports.$plus = $plus;

const $times = (x, y) => y;
exports.$times = $times;

const eleven = (1 + ($times(2, 10)));
exports.eleven = eleven;
