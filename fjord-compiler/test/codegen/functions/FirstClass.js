// module codegen.functions.FirstClass

var toZeroContainerInstance = (ToZeroContainer(toZero2));
exports.toZeroContainerInstance = toZeroContainerInstance;

var toZero = function(x, y) {
  return 0;
};
exports.toZero = toZero;

var toZero2 = toZero;
exports.toZero2 = toZero2;

var ToZeroContainer = function(toZeroField) {
  var _a = {};
  _a.toZeroField = toZeroField;
  return _a;
};
exports.ToZeroContainer = ToZeroContainer;
