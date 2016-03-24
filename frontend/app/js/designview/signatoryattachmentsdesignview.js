var Backbone = require("backbone");
var React = require("react");
var Select = require("../../scripts/common/select");
var _ = require("underscore");
var Button = require("../buttons.js").Button;
var $ = require("jquery");
var Confirmation = require("../confirmations.js").Confirmation;
var FlashMessage = require("../flashmessages.js").FlashMessage;
var SignatoryAttachment = require("../signatoryattachment.js").SignatoryAttachment;

/* This is component for designing signatory attachments
 */

  var DesignSignatoryAttachment = Backbone.Model.extend({
  defaults : {
      name : "",
      description : "",
      signatory : undefined
  },
  name: function() {
       return this.get("name");
  },
  description : function(){
       return this.get("description");
  },
  signatory : function(){
       return this.get("signatory");
  },
  setSignatory: function(signatory) {
      this.set({signatory : signatory});
  },
  setName : function(name) {
      this.set({name : name});
  },
  setDescription : function(description) {
      this.set({description : description});
  },
  ready : function() {
     return this.signatory() != undefined && this.name() != "" && this.description() != "";
  }
});


  var DesignSignatoryAttachments = Backbone.Model.extend({
  defaults : {
      attachments : []
  },
  initialize: function (args) {
      var self = this;
      this.document = args.document;
      var attachments = new Array();
      _.each(args.document.signatories(), function(signatory) {
          _.each(signatory.attachments(), function(attachment) {
              var attachment = new DesignSignatoryAttachment({
                name: attachment.name(),
                description : attachment.description(),
                signatory:  signatory
              });
              attachments.push(attachment);
              self.listenTo(attachment, "change:signatory", function () {
                self.trigger("change:attachments");
              });
          });
      });
      this.set({"attachments":attachments});
  },
  attachments : function(){
       return this.get("attachments");
  },
  addNewAttachment : function(){
        var self = this;
        var attachment = new DesignSignatoryAttachment();
        this.attachments().push(attachment);
        this.listenTo(attachment, "change:signatory", function () {
          self.trigger("change:attachments");
        });
        this.trigger("change:attachments");
  },
  removeAttachment: function(attachment) {
       var newattachments = new Array();
       for(var i=0;i<this.attachments().length;i++)
          if (attachment !== this.attachments()[i])
             newattachments.push(this.attachments()[i]);
       this.set({attachments : newattachments});
       this.stopListening(attachment);
       this.trigger("change:attachments");
  },
  isEmpty: function() {
     return this.attachments().length == 0;
  }
});

/*
 */
var DesignSignatoryAttachmentsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change:attachments', this.render);
        this.model = args.model;
        this.render();
    },
    addAttachmentButton : function() {
        var attachments = this.model;
        return new Button({
            size: 'big',
            text: localization.signatoryAttachments.addAttachment,
            onClick: function() {
                mixpanel.track('Click add sig attachment (popup)');
            attachments.addNewAttachment();
            return false;
            }
        }).el();
    },
    attachmentRow : function(attachment) {
        var attachments = this.model;
        var row = $("<tr/>");
        var td1 = $("<td class='editSignatoryAttachmentTDName'>");
        var editName = $("<input type='text' class='editSignatoryAttachmentName'>");
        editName.val(attachment.name());
        editName.change(function() {
            attachment.setName(editName.val());
            mixpanel.track('Set attachment name');
        });
        td1.append(editName);

        var td2 = $("<td class='editSignatoryAttachmentTDDescription'>");
        var editDesc = $("<textarea class='editSignatoryAttachmentDescription'/>");
        editDesc.val(attachment.description());
        editDesc.change(function() {
            attachment.setDescription(editDesc.val());
            mixpanel.track('Set attachment description');
        });
        td2.append(editDesc);

        var td3 = $("<td class='editSignatoryAttachmentTDSelect'>");
        var nameFromSignatory = function(sig) {
          var text = sig.nameOrEmail();
          if (sig.isCsv())
            text = localization.csv.title;
          if (text == "")
            text = sig.nameInDocument();
          return text;
        };
        var options = [];
        if (!attachment.signatory()) {
          options.push({name:"",value:undefined,disabled:true,selected:true});
        }
        _.each(attachments.document.signatories(), function(sig)  {
          if (sig.signs() && !sig.author()) {
            var text = nameFromSignatory(sig);
            var option = {name: text, value: sig};
            options.push(option);
          }
        });
        var sig = attachment.signatory();
        React.render(React.createElement(Select, {
          isOptionSelected:  function(o) {
            return o.value == sig;
          },
          width: 190,
          options: options,
          onSelect: function(sig) {
            attachment.setSignatory(sig);
            mixpanel.track('Set signatory (in attachment)');
            return true;
          }
        }), td3[0]);

        var td4 = $("<td class='editSignatoryAttachmentTDRemove'>");
        var removeIcon = $("<div class='removeSignatoryAttachmentIcon'>X</div>");
        removeIcon.click(function() {
            attachments.removeAttachment(attachment);
            mixpanel.track('Remove sig attachment');
        });
        td4.append(removeIcon);

        row.append(td1).append(td2).append(td3).append(td4);
        return row;
    },
    render: function () {
        var view = this;
        var attachments = this.model;
        this.container = $(this.el);
        var div = $('<div class="designSignatoryAttachmentsPopupContent"></div>');
        this.container.empty();
        if (!attachments.isEmpty())
        {
            var table= $("<table class='editSignatoryAttachmentTable'/>");
            var th1 = $("<th class='editSignatoryAttachmentTDName'>").text(localization.signatoryAttachments.attachment);
            var th2 = $("<th class='editSignatoryAttachmentTDDescription'>").text(localization.signatoryAttachments.description);
            var th3 = $("<th class='editSignatoryAttachmentTDSelect'>").text(localization.signatoryAttachments.from);
            var th4 = $("<th class='editSignatoryAttachmentTDRemove'>");
            var thead = $("<thead/>").append(th1).append(th2).append(th3).append(th4);

            var tbody = $("<tbody/>");
            _.each(attachments.attachments(), function(a) { tbody.append(view.attachmentRow(a));});

            div.append(table.append(thead).append(tbody));
        }
        this.container.append(div).append(this.addAttachmentButton());
        return this;
    }
});


var DesignSignatoryAttachmentsPopup = exports.DesignSignatoryAttachmentsPopup = function(args) {
         var viewmodel = args.viewmodel;
         var document = viewmodel.document();
         var model = new DesignSignatoryAttachments({ document : document  });
         var view = new DesignSignatoryAttachmentsView({model : model, el : $("<div/>")});
         var popup = new Confirmation({
              title  : localization.signatoryAttachments.requestAttachments,
              content: $("<div>").append($("<div class='modal-subtitle centered'>").html(localization.signatoryAttachments.defineRequests)).append($(view.el)),
              acceptText: localization.save,
              width: 800,
              onAccept : function() {
                  if (_.any(model.attachments(), function(a) {return  !a.ready() })) {
                      new FlashMessage({ type: 'error'
                                       , content: localization.signatoryAttachments.errorFlashMessage});
                      return false;
                  }
                  var uniquelyNamedAttachments = _.uniq(model.attachments(), function(a) {
                    return a.signatory().signatoryid() + a.name();
                  });
                  if (model.attachments().length > uniquelyNamedAttachments.length) {
                    new FlashMessage({type: 'error',
                                      content: localization.signatoryAttachments.uniqueAttachmentNamesError});
                    return false;
                  }
                  _.each(document.signatories(), function(sig) {
                      sig.clearAttachments();
                  });
                  _.each(model.attachments(), function(att) {
                      att.signatory().addAttachment(new SignatoryAttachment({
                          name : att.name(),
                          description: att.description()
                        }));
                  });
                  document.save();
                  document.afterSave(function() {
                      document.recall(function() {
                          document.trigger("change:attachments");
                          viewmodel.saveAndFlashMessageIfAlreadySaved();
                          popup.reject();
                      });
                  });
                  return true;
            }


         });
};


