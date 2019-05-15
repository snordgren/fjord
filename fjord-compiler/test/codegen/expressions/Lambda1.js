// module codegen.expressions.Lambda1

var lambdaArg = function (f) {
  return (f(3));
};
exports.lambdaArg = lambdaArg;

var useLambdaArg = (lambdaArg(function(x) {
  return 0;
}));
exports.useLambdaArg = useLambdaArg;
