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
