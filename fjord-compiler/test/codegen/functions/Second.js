// module test.codegen.functions.Second

const second = (x, y) => y;
exports.second = second;

const two = (second(1, 2));
exports.two = two;
