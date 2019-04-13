// module test.codegen.expressions.Match

export const $TagZero = 1;

export const Zero = Object.freeze([$TagZero]);

export const $TagOne = 2;

export const One = Object.freeze([$TagOne]);

export const toInt = b => (() => {
  const tag = b[0];
  if (tag === $TagZero) {
    return 0;
  } else if (tag === $TagOne) {
    return 1;
  }
})();

export const zero = (toInt(Zero));

export const one = (toInt(One));
