// module test.codegen.functions.Curry

export const first = (x, y, z) => x;

export const alwaysOne = (_0, _1) => (first(1, _0, _1));

export const alwaysTwo = (x, _0) => (first(2, x, _0));

export const one = (alwaysOne(2, 3));

export const two = (alwaysTwo(3, 4));
