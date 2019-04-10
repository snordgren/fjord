// module test.codegen.functions.Second

export const second = x => y => y;

export const two = ((second(1))(2));
