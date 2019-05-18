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

var maybeOne = function() {
  return (Some(1));
};
exports.maybeOne = maybeOne;
