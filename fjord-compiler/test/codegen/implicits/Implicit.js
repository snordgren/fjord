// module test.codegen.implicits.Implicit

const Zero = zero => {
  var _a = {};
  _a.zero = zero;
  return _a;
};
exports.Zero = Zero;

const zeroInstance = (Zero(0));
exports.zeroInstance = zeroInstance;

const zero = anything => {
  var ev = zeroInstance;
  return 0;
};
exports.zero = zero;
