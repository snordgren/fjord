// module codegen.implicits.ImplicitAdd

var Add = function(add) {
  var _a = {};
  _a.add = add;
  return _a;
};
exports.Add = Add;

var addInt = function(x, y) {
  return 0;
};
exports.addInt = addInt;

var addString = function(x, y) {
  return "";
};
exports.addString = addString;

var intInstance = (Add(addInt));
exports.intInstance = intInstance;

var stringInstance = (Add(addString));
exports.stringInstance = stringInstance;

var $plus$plus = function(ev, x, y) {
  return (ev.add(x, y));
};
exports.$plus$plus = $plus$plus;

var three = ($plus$plus(intInstance, 1, 2));
exports.three = three;

var helloWorld = ($plus$plus(stringInstance, "hello, ", "world"));
exports.helloWorld = helloWorld;
