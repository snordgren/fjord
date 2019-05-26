// module codegen.operators.Addition1

var $plus = function(x, y) {
  return 0;
};
exports.$plus = $plus;

var usePlus = function(x, y) {
  return ($plus(x, y));
};
exports.usePlus = usePlus;

var myFive = (usePlus(2, 3));
exports.myFive = myFive;
