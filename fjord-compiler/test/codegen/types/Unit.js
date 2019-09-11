// module codegen.types.Unit

var TestUtil = require("../../../test/node_modules/@fjord-lang/test-util/dist/Util.js");

var UnitContainer = function(value) {
  var _a = {};
  _a.value = value;
  return _a;
};
exports.UnitContainer = UnitContainer;

var unitId = function(a) {
  return (TestUtil.rm(a, Object.freeze([])));
};
exports.unitId = unitId;
