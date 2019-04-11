// module test.codegen.types.Point

export const Point = (x, y) => ({ x, y });

export const defaultPosition = (Point(1, 2));
