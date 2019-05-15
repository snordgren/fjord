// module test.codegen.expressions.Match

var $TagZero = 1;
exports.$TagZero = $TagZero;

var $TagOne = 2;
exports.$TagOne = $TagOne;

var Zero = Object.freeze([$TagZero]);
exports.Zero = Zero;

var One = Object.freeze([$TagOne]);
exports.One = One;

var toInt = function(b) {
  return (function() {
    var target = b;
    var tag = (target)[0];
    if ((target === $TagZero)) {
      return 0;
    } else if ((target === $TagOne)) {
      return 1;
    }
  })();
};
exports.toInt = toInt;

var zero = (toInt(Zero));
exports.zero = zero;

var one = (toInt(One));
exports.one = one;
