/* Whole page attachment view
 */


(function(window) {


var Attachment = Backbone.Model.extend({
  initialize: function(args) {
    var attachment = this;
    this.set({"document" : new Document({id: args.documentid})})
    this.document().bind("change", function() {attachment.trigger('change') });
    this.document().bind("reset", function() {attachment.trigger('reset') });
    this.document().recall();
  },
  ready : function()  {
    return this.document().ready();  
  },
  document : function() {
    return this.get("document");  
  }
});

var AttachmentView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.render();
  },
  render: function() {
    var attachment = this.model;
    $(this.el).empty();
    if (!attachment.ready()) return;
    this.file = KontraFile.init({
                    file: attachment.document().mainfile(),
                    document: attachment.document()
                });
    $(this.el).append(this.file.view.el)
  }
});


window.KontraAttachment = function(args) {
    var model = new Attachment(args);
    var view = new AttachmentView({
      model: model,
      el: $("<div/>")
    });
    return {
        model : function() { return model;},
        view : function()  { return view; }
     }
};
})(window);
