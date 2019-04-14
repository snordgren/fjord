// module test.codegen.types.MaybeInt

export const $TagNone = 1;

export const $TagSome = 2;

export const None = Object.freeze([$TagNone]);

export const Some = _0 => Object.freeze([$TagSome, _0]);
