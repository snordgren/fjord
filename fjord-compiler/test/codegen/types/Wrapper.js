// module codegen.types.Wrapper

var Wrapper = function(value) {
  var _a = {};
  _a.value = value;
  return _a;
};
exports.Wrapper = Wrapper;

var wrapInt = function(x) {
  return (Wrapper(x));
};
exports.wrapInt = wrapInt;
