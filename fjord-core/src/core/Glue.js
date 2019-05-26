// module core.Glue

var addInt = function (x, y) {
  return ((x|0) + (y|0))|0;
}
exports.addInt = addInt;

var subInt = function (x, y) {
  return ((x|0) - (y|0))|0;
}
exports.subInt = subInt;

var mulInt = function (x, y) {
  return ((x|0) * (y|0))|0;
}
exports.mulInt = mulInt;

var divInt = function (x, y) {
  return ((x|0) / (y|0))|0;
}
exports.divInt = divInt;

var concatString = function (x, y) {
  return x + y;
}
exports.concatString = concatString;

var boolEquals = function (x, y) {
  if (x === y) {
    return Object.freeze[1];
  } else {
    return Object.freeze[2];
  }
};
exports.boolEquals = boolEquals;
