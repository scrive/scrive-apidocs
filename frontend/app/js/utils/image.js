exports.ImageUtil = {
  // Returns base64 encoded png image date from canvas,
  // after setting transparent background and adding opacity.
  // Result is passed as an argument to callback function
  addTransparentBGAndSerializeCanvas: function(canvas, width, height, opacity, callback) {
    var tmpImage = new Image();
    tmpImage.type = 'image/png';
    tmpImage.src = canvas.toDataURL('image/png', 1);

    // callback for Image.onload (parametrized by timeout value)
    // we have to wait inside it a bit, because IE9 calls this
    // too soon, and image is still empty
    var serializer = function(timeout) {
      var tmpCanvas = $('<canvas />').attr('width', width).attr('height', height)[0];
      var context = tmpCanvas.getContext('2d');
      context.fillStyle = 'rgba(0, 0, 0, 0)';
      context.fillRect(0, 0, width, height);
      context.drawImage(tmpImage, 0, 0, width, height);
      var imageIsEmpty = true;
      var imageData = context.getImageData(0, 0, width, height);
      var data = imageData.data;
      for (var i = 0; i < data.length; i += 4) {
        data[i+3] = data[i+3] * opacity;
        if(data[i] || data[i+1] || data[i+2] || data[i+3]) {
          imageIsEmpty = false;
        }
      }
      context.putImageData(imageData, 0, 0);

      if (imageIsEmpty) {
        // try again with a bigger timeout period
        setTimeout(function() {
          serializer(timeout + 100);
        }, timeout);
      } else {
        callback(tmpCanvas.toDataURL('image/png', 1));
      }
    };
    tmpImage.onload = function() {
      serializer(0);
    };
  }
};
