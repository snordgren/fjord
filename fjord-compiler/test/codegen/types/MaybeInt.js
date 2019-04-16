// module test.codegen.types.MaybeInt

export const $TagNone = 1;

export const $TagSome = 2;

export const None = Object.freeze([$TagNone]);

export const Some = _0 => Object.freeze([$TagSome, _0]);

export const maybeToInt = m => (() => {
  var tag;
  var target;
  target = m;
  tag = (target)[0];
  if ((target === $TagNone)) {
    return 0;
  } else if ((target === $TagSome)) {
    var a;
    a = (target)[1];
    return a;
  }
})();
