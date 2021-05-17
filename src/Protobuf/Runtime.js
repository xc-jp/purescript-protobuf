"use strict";

exports.unsafeArrayPush = function (xs) {
  return function (ys) {
    return xs.push.apply(xs, ys);
  };
};

exports.unsafeCopyFloat32 = function(dv) {
  return function (n) {
    let result = new Array(n);
    for (let i=0;i<n;i=i+1) {
      result[i] = dv.getFloat32(i*4, true);
    }
    return result;
  };
};