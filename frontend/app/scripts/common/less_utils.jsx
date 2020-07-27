import camelCase from "lodash/camelCase";

function toJsVars (obj) {
  const output = {};

  for (const key in obj) {
    const value = obj[key];
    output[camelCase(key)] = /^\d+(\.\d+)?px$/.test(value) ? parseFloat(value) : value;
  }

  return output;
}

/**
 * Convert LESS variable objects to provide compatibility between
 * less-vars-loader (currently used) and less-interop-loader (retired).
 */
function toLessInteropLoader(vars) {
  const transformed = toJsVars(vars);
  // console.log("interop transformed vars:", transformed, vars);
  return transformed;
}

module.exports = {
  toLessInteropLoader: toLessInteropLoader
}
