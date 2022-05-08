export const unsafeArrayPush = function (xs) {
  return function (ys) {
    return xs.push.apply(xs, ys);
  };
};
