// module test.codegen.functions.First

const first = (x, y) => x;
exports.first = first;

const one = (first(1, 2));
exports.one = one;
