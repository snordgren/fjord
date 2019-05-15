// module test.codegen.functions.Identity

var id = function(x) {
  return x;
};
exports.id = id;

var zero = (id(0));
exports.zero = zero;
