// module test.codegen.types.Point

export const Point = (x, y) => {
  var _a;
  _a = {};
  _a.x = x;
  _a.y = y;
  return _a;
};

export const defaultPosition = (Point(1, 2));

export const toOrigin = p => (() => {
  var _m;
  _m = p;
  _m.x = 0;
  _m.y = 0;
  return _m;
})();

export const setXAndY = (x, y, p) => (() => {
  var _m;
  _m = (() => {
    var _m;
    _m = p;
    _m.x = x;
    return _m;
  })();
  _m.y = y;
  return _m;
})();
