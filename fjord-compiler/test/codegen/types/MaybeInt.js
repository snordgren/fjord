// module test.codegen.types.MaybeInt

var $TagNone = 1;
exports.$TagNone = $TagNone;

var $TagSome = 2;
exports.$TagSome = $TagSome;

var None = Object.freeze([$TagNone]);
exports.None = None;

var Some = _0 => Object.freeze([$TagSome, _0]);
exports.Some = Some;

var maybeToInt = m => (() => {
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
