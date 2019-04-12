// module test.codegen.types.Point

export const Point = (x, y) => ({ x, y });

export const defaultPosition = (Point(1, 2));

export const toOrigin = p => (() => {
  const _m = p;
  _m.x = 0;
  _m.y = 0;
  return _m;
})();

export const setXAndY = (x, y, p) => (() => {
  const _m = (() => {
    const _m = p;
    _m.x = x;
    return _m;
  })();
  _m.y = y;
  return _m;
})();
