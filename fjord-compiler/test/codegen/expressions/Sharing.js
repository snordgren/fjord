// module codegen.expressions.Sharing

var useSharedInt = function(x, y) {
  return y;
};
exports.useSharedInt = useSharedInt;

var shareInt = function(x) {
  return (useSharedInt(x, x));
};
exports.shareInt = shareInt;

var myInt = (shareInt(5));
exports.myInt = myInt;
