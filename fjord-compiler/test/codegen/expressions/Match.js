// module test.codegen.expressions.Match

export const $TagZero = 1;

export const $TagOne = 2;

export const Zero = Object.freeze([$TagZero]);

export const One = Object.freeze([$TagOne]);

export const toInt = b => (() => {
  var tag;
  var target;
  target = b;
  tag = (target)[0];
  if ((target === $TagZero)) {
    return 0;
  } else if ((target === $TagOne)) {
    return 1;
  }
})();

export const zero = (toInt(Zero));

export const one = (toInt(One));
