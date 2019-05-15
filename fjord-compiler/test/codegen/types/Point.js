// module test.codegen.types.Point

var Point = (x, y) => {
  var _a = {};
  _a.x = x;
  _a.y = y;
  return _a;
};
exports.Point = Point;

var defaultPosition = (Point(1, 2));
exports.defaultPosition = defaultPosition;

var toOrigin = p => (() => {
  var _m = p;
  _m.x = 0;
  _m.y = 0;
  return _m;
})();
exports.toOrigin = toOrigin;

var setXAndY = (x, y, p) => (() => {
  var _m = (() => {
    var _m = p;
    _m.x = x;
    return _m;
  })();
  _m.y = y;
  return _m;
})();
exports.setXAndY = setXAndY;
