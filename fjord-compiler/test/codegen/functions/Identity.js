// module test.codegen.functions.Identity

const id = x => x;
exports.id = id;

const zero = (id(0));
exports.zero = zero;
