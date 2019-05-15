// module test.codegen.operators.Precedence

var $plus = function(x, y) {
  return y;
};
exports.$plus = $plus;

var $times = function(x, y) {
  return y;
};
exports.$times = $times;

var eleven = (1 + ($times(2, 10)));
exports.eleven = eleven;
