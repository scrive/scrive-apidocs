/* Whole page attachment view
 */


(function(window) {


var Attachment = Backbone.Model.extend({
  defaults: {
    id: 0,
    title: ""
  },
  initialize: function(args) {
    this.url = "/att/" + args.id;
  },
  parse: function(args) {
    return {
      id: args.id,
      title: args.title,
      file: new File({id: args.file, attachmentid: this.get("id"), name: args.title })
    };
  },
  title : function() {
    return this.get("title");
  },
  file : function() {
    return this.get("file");
  },
  ready: function() {
    return this.has("file");
  },
  id: function() {
    return this.get("id");
  },
  rename : function(name, f) {
    var self = this;
    var attachmentid = this.get("id");
    return new Submit({
      url : "/a/rename/" + attachmentid,
      docname : name,
      method : "POST",
      ajaxSuccess : function() {
        self.set({title: name}, {silent: true});
        f();
      },
      ajax : true
    });
  },
  recall: function() {
    this.fetch({
      processData: true,
      cache: false
    });
  }
});

var AttachmentView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.editable = args.editable;
    this.editing = false;
    this.render();
  },
  downloadbox : function() {
    var box = $("<span class='download'>");
    var link = $("<a  target='_blank'/>").attr("href",this.model.file().downloadLink()).text(localization.downloadPDF);
    return box.append(link);
  },
  namebox : function() {
    var view = this;
    var attachment = this.model;
    var box =  $("<div id='signStepsTitleRowTextContainer'/>");
    box.append($("<span class='title'/>").text(localization.attachmentType));
    var titlebox = $("<span class='docname'>");
    box.append(titlebox);
    if (!this.editable) {
      titlebox.append($("<span class='visible-docname'></span>").text(attachment.title()));
    }
    else if (!this.editing)
    {
      var editIcon = $("<a href='#' class='icon edit' style='margin-right: 2px'></a>");
      editIcon.click(function() {view.editing = true; view.render(); return false;});
      titlebox.append(editIcon);
      titlebox.append($("<span class='visible-docname'></span>").text(attachment.title()));
    }
    else
    {
      var acceptIcon = $("<a href='#' class='icon small ok submitafterdone' style='margin: 2px;'></a>");
      this.input = $("<input type='text' name='docname' class='docname-edit' />").val(attachment.title());
      var changeFunction = function() {
        attachment.rename(view.input.val(), function() {view.editing = false;view.render();}).send();
        return false;
      };
      acceptIcon.click(function() {return changeFunction()});
      this.input .keypress(function(event) {
        if(event.which === 13)
          return changeFunction();
      });
      titlebox.append(acceptIcon);
      titlebox.append(this.input);
    }
    return box;
  },
  render: function() {
    var attachment = this.model;
    $(this.el).empty();
    if( !attachment.ready()) {
      return;
    }

    var titlerow = $("<div id='signStepsTitleRow'/>");
    titlerow.append(this.namebox());
    titlerow.append(this.downloadbox());

    $(this.el).append(titlerow);
    if (this.editing && this.input != undefined)
      this.input.focus();

    this.file = KontraFile.init({
      file: attachment.file()
    });


    $(this.el).append(this.file.view.el);
  }
});


window.KontraAttachment = function(args) {
  var model = new Attachment(args);
  model.recall();
  var view = new AttachmentView({
    model: model,
    editable : args.editable,
    el: $("<div/>")
  });
  return {
    model : function() { return model;},
    view : function()  { return view; }
  };
};

})(window);
