export const unsafeCopyFloat32le = function(dv) {
  return function (n) {
    let result = new Array(n);
    for (let i=0;i<n;i=i+1) {
      result[i] = dv.getFloat32(i*4, true);
    }
    return result;
  };
};

export const unsafeCopyFloat64le = function(dv) {
  return function (n) {
    let result = new Array(n);
    for (let i=0;i<n;i=i+1) {
      result[i] = dv.getFloat64(i*8, true);
    }
    return result;
  };
};

export const unsafeCopyInt32le = function(dv) {
  return function (n) {
    let result = new Array(n);
    for (let i=0;i<n;i=i+1) {
      result[i] = dv.getInt32(i*4, true);
    }
    return result;
  };
};

export const unsafeCopyUInt32le = function(dv) {
  return function (n) {
    let result = new Array(n);
    for (let i=0;i<n;i=i+1) {
      result[i] = dv.getUint32(i*4, true);
    }
    return result;
  };
};

export const unsafeCopyInt64le = function(lowHighConstructor) {
  return function(dv) {
    return function (n) {
      let result = new Array(n);
      for (let i=0;i<n;i=i+1) {
        let low = dv.getInt32(i*4, true);
        let high = dv.getInt32((i+1)*4, true);
        result[i] = lowHighConstructor(low, high);
      }
      return result;
    };
  };
};
