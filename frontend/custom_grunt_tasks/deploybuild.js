'use strict';

var fs = require('fs'),
  path = require('path'),
  crypto = require('crypto'),
  generateVersionId = require('./utils/version_id_generator');

module.exports = function(grunt) {
  grunt.registerMultiTask('deploybuild', 'Prefix static asset file names with a content hash and replace includes of them in index.html', function() {
    
    var cssFiles = [],
    jsFiles = [];

    // Rename files with to have versionId in their name
    this.files.forEach(function(filePair) {
      filePair.src.forEach(function(f) {
        var versionId = generateVersionId(),
        renamed = [versionId, path.basename(f)].join('.'),
        outPath = path.resolve(path.dirname(f), renamed);

	if(renamed.indexOf('.css') != -1) {
	  cssFiles.push(renamed);
	} else if(renamed.indexOf('.js') != -1) {
	  jsFiles.push(renamed);
	}

        fs.renameSync(f, outPath);
        grunt.log.write(f + ' ').ok(renamed);
      });
    });
  });
};
