// module codegen.functions.OverlapTypeVars

var Util = require("../../../test/typedef/Util.js");

var last = function(x, y) {
  return (Util.drop(y, x));
};
exports.last = last;

var id = function(x) {
  return x;
};
exports.id = id;

var overlapTypeVars = (last(0, (id("str"))));
exports.overlapTypeVars = overlapTypeVars;
