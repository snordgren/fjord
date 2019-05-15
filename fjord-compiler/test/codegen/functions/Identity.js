// module test.codegen.functions.Identity

var id = x => x;
exports.id = id;

var zero = (id(0));
exports.zero = zero;
