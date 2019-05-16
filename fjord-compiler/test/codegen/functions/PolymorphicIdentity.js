// module codegen.functions.PolymorphicIdentity

var id = function(x) {
  return x;
};
exports.id = id;

var five = function() {
  return (id(5));
};
exports.five = five;

var helloWorld = function() {
  return (id("hello, world!"));
};
exports.helloWorld = helloWorld;

var pos = function() {
  return (id(Object.freeze([5, 4])));
};
exports.pos = pos;
