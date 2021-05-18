exports._decodeArray = function(f) {
  return function(n) {
    return function () {
      var a = new Array(n);
      var l = a.length;
      var i;
      for (i=0;i<l;i+=1) {
        a[i] = f(i)();
      }
      return a;
    };
  };
}

// Check whether the number 1 updates
// the first or last byte in a 4 byte array.
// This determines endianness
exports._isBigEndian = function() {
    const array = new Uint8Array(4);
    const view = new Uint32Array(array.buffer);
    return !((view[0] = 1) & array[0]);
}

