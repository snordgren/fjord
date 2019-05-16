var Core = require('../bin/Core');

test('addition' , () => {
  expect(Core.$plus(1, 2)).toBe(3);
  expect(Core.$plus(1.1, 2)).toBe(3);
});

test('subtraction', () => {
  expect(Core.$minus(3, 2)).toBe(1);
  expect(Core.$minus(3.3, 2)).toBe(1);
});

test('multiplication', () => {
  expect(Core.$times(4, 5)).toBe(20);
  expect(Core.$times(4.4, 5)).toBe(20);
});

test('division', () => {
  expect(Core.$div(8.9, 2)).toBe(4);
  expect(Core.$div(3, 2)).toBe(1);
});

test('string concat', () => {
  expect(Core.$plus$plus ('hello, ', 'world')).toBe('hello, world');
});
