// module test.codegen.implicits.Implicit

var Zero = function(zero) {
  var _a = {};
  _a.zero = zero;
  return _a;
};
exports.Zero = Zero;

var zeroInstance = (Zero(0));
exports.zeroInstance = zeroInstance;

var zero = function(ev, anything) {
  return 0;
};
exports.zero = zero;

var toZero = (zero(5));
exports.toZero = toZero;
