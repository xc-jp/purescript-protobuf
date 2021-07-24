"use strict";

exports.unsafeArrayPush = function (xs) {
  return function (ys) {
    return xs.push.apply(xs, ys);
  };
};