// module codegen.functions.PolymorphicIdentity

var id = function(x) {
  return x;
};
exports.id = id;

var five = (id(5));
exports.five = five;

var helloWorld = (id("hello, world!"));
exports.helloWorld = helloWorld;

var pos = (id(Object.freeze([5, 4])));
exports.pos = pos;
