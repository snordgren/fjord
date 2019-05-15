// module test.codegen.operators.Precedence

var $plus = (x, y) => y;
exports.$plus = $plus;

var $times = (x, y) => y;
exports.$times = $times;

var eleven = (1 + ($times(2, 10)));
exports.eleven = eleven;
