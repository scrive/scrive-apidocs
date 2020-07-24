function parseIntPixels (obj) {
  const output = {};

  for (const key in obj) {
    output[key] = obj[key].match(/\dpx$/) ? parseInt(obj[key]) : obj[key];
  }

  return output;
}

function camelCase(obj) {
  const output = {};

  for (const key in obj) {
    const newKey = key.toLowerCase().replace(/([-_][a-z])/g, group =>
      group.toUpperCase().replace('-', '').replace('_', '')
    );

    output[newKey] = obj[key];
  }

  return output;
}

/**
 * Convert LESS variable objects to provide compatibility between
 * less-vars-loader (currently used) and less-interop-loader (retired).
 */
function toLessInteropLoader(vars) {
  return parseIntPixels(camelCase(vars));
}

module.exports = {
  toLessInteropLoader: toLessInteropLoader
}
