// module test.codegen.implicits.Implicit

export const Zero = zero => {
  var _a = {};
  _a.zero = zero;
  return _a;
};

export const zeroInstance = (Zero(0));

export const zero = anything => {
  var ev = zeroInstance;
  return 0;
};
