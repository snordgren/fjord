// module codegen.operators.Addition

var $plus = function(x, y) {
  return y;
};
exports.$plus = $plus;

var three = (1 + 2);
exports.three = three;

var seven = ((1 + 2) + 4);
exports.seven = seven;
