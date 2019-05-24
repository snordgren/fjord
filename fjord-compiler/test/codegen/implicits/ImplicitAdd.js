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

var intInstance = (Add(addInt));
exports.intInstance = intInstance;
