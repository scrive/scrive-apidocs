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

    // Create html code to include all production - css and js files
    var includes = [];
    jsFiles.forEach(function(str) {
      includes.push(['<script src="/', str, '"></script>'].join(''));
    });
    cssFiles.forEach(function(str) {
      includes.push(['<link rel="stylesheet" href="/', str, '">'].join(''));
    });
    
    // Create new html file for production
    var newLines = [],
    inBuildBlock = false;
    fs.readFileSync(this.data.htmlFile).toString().split('\n').forEach(function (line) { 
      if(!inBuildBlock && line.indexOf('buildfordev') != -1) {
	// found build for dev block

	inBuildBlock = true;
	newLines = newLines.concat(includes);	
      } else if(line.indexOf('endbuildfordev') != -1) {
	// found end of build for dev block
	inBuildBlock = false;
      } else if(!inBuildBlock) {
	// outside build for dev block
	newLines.push(line);
      }
    });

    // Replace old html file with the in-memory generated html
    fs.unlinkSync(this.data.htmlFile);
    newLines.forEach(function(line) {
      fs.appendFileSync(this.data.htmlFile, line + '\n');
    }.bind(this));
    grunt.log.write('Replaced html file >> ', this.data.htmlFile);
  });
};
