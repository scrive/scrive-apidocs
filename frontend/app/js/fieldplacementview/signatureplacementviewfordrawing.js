define(['Backbone', 'legacy_code'], function(Backbone) {

window.SignaturePlacementViewForDrawing = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear);
        this.model.bind('change', this.render);
        this.height = args.height;
        this.width = args.width;
        this.arrow = args.arrow;
        this.signview = args.signview;

        this.render();
    },
    clear: function() {
        this.off();
        this.model.unbind('removed', this.clear);
        this.model.unbind('change', this.render);
        $(this.el).remove();
    },
    updateSize: function(width, height) {
      this.width = width;
      this.height = height;
      var box = $(this.el);
      box.width(width);
      box.height(height);
      box.css("line-height",Math.floor(height) + "px");
    },
    render: function() {
            var self = this;
            var field = this.model;
            var box = $(this.el);
            var width =  this.width;
            var height = this.height;
            var arrow = this.arrow;
            var signview = this.signview;
            var image = field.value();
            box.empty();
            box.unbind("click");
            box.attr("style","");
            box.addClass('signatureBox').addClass('forDrawing');
            if (image == "")
            {
                box.removeClass('withImage').addClass('signaturePlaceholder');
                if (field.obligatory())
                  box.addClass("obligatory");
                else
                  box.addClass("optional");

                box.width(width);
                box.height(height);
                box.css("line-height",Math.floor(height) + "px");
                box.text(localization.signature.clickToDraw);
            }
            else {
                console.log("Place for drawing - rendering with value");
                box.removeClass("signaturePlaceholder").addClass('withImage');
                var img = $("<img alt=''/>");
                box.css("width",width);
                box.css("height",height);
                if(BrowserInfo.isIE7orLower() && image.substring(0, 'data:'.length) == 'data:') {
                  // IE 7 cannot handle data uris
                  img.attr('src', '/img/img-signature-saved-' + localization.code + '.png');
                } else {

                  img.css("width",width);
                  img.attr("width",width);
                  img.css("height",height);
                  img.attr("height",height);
                  img.attr('src', image);
                }
                box.append(img);
            }

            box.click(function() {
              new SignatureDrawOrTypeModal({
                field: field,
                width: self.width,
                height: self.height,
                arrow: arrow,
                signview: signview
              });
            });
            return this;
    }
});

});
