// module test.codegen.expressions.Match

const $TagZero = 1;
exports.$TagZero = $TagZero;

const $TagOne = 2;
exports.$TagOne = $TagOne;

const Zero = Object.freeze([$TagZero]);
exports.Zero = Zero;

const One = Object.freeze([$TagOne]);
exports.One = One;

const toInt = b => (() => {
  var target = b;
  var tag = (target)[0];
  if ((target === $TagZero)) {
    return 0;
  } else if ((target === $TagOne)) {
    return 1;
  }
})();
exports.toInt = toInt;

const zero = (toInt(Zero));
exports.zero = zero;

const one = (toInt(One));
exports.one = one;
