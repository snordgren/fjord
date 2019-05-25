// module test.codegen.types.Point

var Point = function(x, y) {
  var _a = {};
  _a.x = x;
  _a.y = y;
  return _a;
};
exports.Point = Point;

var defaultPosition = (Point(1, 2));
exports.defaultPosition = defaultPosition;

var toOrigin = function(p) {
  return (function() {
    var _m = p;
    _m.x = 0;
    _m.y = 0;
    return _m;
  })();
};
exports.toOrigin = toOrigin;

var setXAndY = function(x, y, p) {
  return (function() {
    var _m = (function() {
      var _m = p;
      _m.x = x;
      return _m;
    })();
    _m.y = y;
    return _m;
  })();
};
exports.setXAndY = setXAndY;

var withX = function(p, f) {
  return (f(p.x));
};
exports.withX = withX;
