// module codegen.types.Maybe

var $TagNone = 1;
exports.$TagNone = $TagNone;

var $TagSome = 2;
exports.$TagSome = $TagSome;

var None = Object.freeze([$TagNone]);
exports.None = None;

var Some = function(_0) {
  return Object.freeze([$TagSome, _0]);
};
exports.Some = Some;

var maybeNone = None;
exports.maybeNone = maybeNone;

var maybeOne = (Some(1));
exports.maybeOne = maybeOne;

var someOrZero = function(x) {
  return (function() {
    var target = x;
    var tag = (target)[0];
    if ((target === $TagNone)) {
      return 0;
    } else if ((target === $TagSome)) {
      var y = (target)[1];
      return y;
    }
  })();
};
exports.someOrZero = someOrZero;

var one = (someOrZero(maybeOne));
exports.one = one;
