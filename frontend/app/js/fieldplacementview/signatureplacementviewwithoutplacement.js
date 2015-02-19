define(['Backbone', 'legacy_code'], function(Backbone) {

window.SignaturePlacementViewWithoutPlacement = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.width = args.width;
        this.height = args.height;
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    ddIcon : function() {
        return $("<div class='signatureDDIcon'/>");
    },
    header : function() {
        var field = this.model;
        var signatory = this.model.signatory();
        var box = $("<div class='signatureHeader'>");
        var sname = signatory.nameOrEmail();
        if (sname == "")
        {
            if (signatory.isCsv())
             sname =  localization.csv.title;
            else
             sname =  localization.process.signatoryname + " " + signatory.signIndex();
        }
        var text = $("<span>" + localization.signature.placeFor + "</span>");
        text.find(".put-name-here").text(sname);
        box.append(text);
        return box;
    },
    render: function() {
            var box = $(this.el);
            box.empty();
            var width =  this.width != undefined ? this.width : 260;
            var height = this.height != undefined ? this.height : 102;
            box.addClass('signatureBox');
            box.append(this.ddIcon());
            box.append(this.header());
            box.width(width);
            box.height(height);
            return this;
    }
});

});
