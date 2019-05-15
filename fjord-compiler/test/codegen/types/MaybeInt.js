// module test.codegen.types.MaybeInt

const $TagNone = 1;
exports.$TagNone = $TagNone;

const $TagSome = 2;
exports.$TagSome = $TagSome;

const None = Object.freeze([$TagNone]);
exports.None = None;

const Some = _0 => Object.freeze([$TagSome, _0]);
exports.Some = Some;

const maybeToInt = m => (() => {
  var target = m;
  var tag = (target)[0];
  if ((target === $TagNone)) {
    return 0;
  } else if ((target === $TagSome)) {
    var a = (target)[1];
    return a;
  }
})();
exports.maybeToInt = maybeToInt;
