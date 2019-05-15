// module test.codegen.types.Tuple

var createTup2 = function(x, y) {
  return Object.freeze([x, y]);
};
exports.createTup2 = createTup2;

var tup2Inst = (createTup2(1, 2));
exports.tup2Inst = tup2Inst;

var createTup3 = function(x, y, z) {
  return Object.freeze([x, y, z]);
};
exports.createTup3 = createTup3;

var tup3Inst = (createTup3(3, 4, 5));
exports.tup3Inst = tup3Inst;

var createTup4 = function(x, y, z, w) {
  return Object.freeze([x, y, z, w]);
};
exports.createTup4 = createTup4;

var tup4Inst = (createTup4(6, 7, 8, 9));
exports.tup4Inst = tup4Inst;
