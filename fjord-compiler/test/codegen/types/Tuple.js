// module test.codegen.types.Tuple

export const createTup2 = (x, y) => Object.freeze([x, y]);

export const tup2Inst = (createTup2(1, 2));

export const createTup3 = (x, y, z) => Object.freeze([x, y, z]);

export const tup3Inst = (createTup3(3, 4, 5));

export const createTup4 = (x, y, z, w) => Object.freeze([x, y, z, w]);

export const tup4Inst = (createTup4(6, 7, 8, 9));
