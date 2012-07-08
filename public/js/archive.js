/* Arrows
 * There are 4 types of arrows: point-left, point-right, scroll-up, scroll-down
*/

(function(window){



var DocumentsListDefinition = {
    name : "Documents Table",
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Document" },
    sorting: new Sorting({ fields: ["title", "status", "time", "party", "author"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.documents.search}),
    selectfiltering : [
        new SelectFiltering({description: localization.filterByProcess.showAllProcesses, name: "process",
                             options: [ {name: localization.filterByProcess.showContractsOnly, value: "contract"},
                                        {name: localization.filterByProcess.showOffersOnly,    value: "offer"},
                                        {name: localization.filterByProcess.showOrdersOnly,    value: "order"}
                                      ]})
        ],
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: localization.archive.documents.columns.status, width:"40px", field:"status",
                 rendering: function(status,idx,listobject) {
                    var icon = jQuery("<div class='icon status "+status+"'></div>")
                    var tip = jQuery("<div id='tooltip-"+status+"'> <div class='icon status "+status+"'></div><p>"+
                                      localization.statusToolTip[status]+"</p></div>");
                    ToolTip.set({
                        on: icon,
                        tip: tip
                    })
                    if ((listobject.field("anyinvitationundelivered") == "True" &&  idx == undefined)
                       || (idx != undefined && listobject.subfield(idx,"invitationundelivered") == "True"))
                       icon = jQuery.merge( icon, jQuery("<span style='color:#000000;position:relative;top:-2px'>!</span>"));

                    return icon;
                 }
        }),
        new Cell({name: localization.archive.documents.columns.time, width:"116px", field:"time"}),
        new Cell({name: localization.archive.documents.columns.sender, width:"110px", field:"author",  special: "link"}),
        new Cell({width:"5px" }),
        new Cell({name: localization.archive.documents.columns.party, width:"200px", field:"party", special: "expandable", subfield : "name"}),
        new Cell({width:"5px" }),
        new Cell({name: localization.archive.documents.columns.title, width:"250px", field:"title",  special: "link"}),
        new Cell({width:"5px" }),
        new Cell({name: localization.archive.documents.columns.type, width:"40px", field:"process",
                  rendering: function(value, _idx, _model) {
                      var txt = "";
                      if( localization.process[value] !== undefined ) {
                          txt = localization.process[value].shortName;
                      }
                      return jQuery("<span />").text(txt);
                    }
                 })
        ],
    options : [{name : localization.archive.documents.sendreminder.action,
                onSelect: function(docs){
                            allSendOrOpenSelected = _.all(docs, function(doc) {
                                                return doc.field("status") == "sent"      ||
                                                       doc.field("status") == "delivered" ||
                                                       doc.field("status") == "read"      ||
                                                       doc.field("status") == "opened"
                                            })
                             if (!allSendOrOpenSelected) {
                                FlashMessages.add({content: localization.cantSendReminder(localization.documents.toLowerCase()), color: "red"});
                                return;
                             }
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                remind: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                             var content = jQuery("<p/>");
                             if (docs.length == 1) {
                               content.append(localization.archive.documents.sendreminder.bodysingle + " ");
                               content.append(jQuery("<strong/>").text(docs[0].field("title")));
                               content.append("?");
                             } else {
                               content.text(localization.archive.documents.sendreminder.bodymulti + " "+ docs.length + (" " + localization.documents +"?").toLowerCase());
                             }
                             Confirmation.popup({
                                submit: submit,
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.documents.sendreminder.action,
                                content: content
                              })
                          }
               },
               {name : localization.archive.documents.delete.action,
                onSelect: function(docs){
                            anySendOrOpenSelected = _.any(docs, function(doc) {
                                                return doc.field("status") == "sent"      ||
                                                       doc.field("status") == "delivered" ||
                                                       doc.field("status") == "read"      ||
                                                       doc.field("status") == "opened"    ||
                                                       doc.field("anyinvitationundelivered") == "True"
                                            })
                             if (anySendOrOpenSelected) {
                                FlashMessages.add({content: localization.archive.documents.delete.cantremoveactive, color: "red"});
                                return;
                             }
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                remove: "true",
                                                archive: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                             var confirmtext = jQuery("<p/>").append(localization.archive.documents.delete.body + " ");
                             var label = jQuery("<strong/>");
                             if (docs.length == 1) {
                               confirmtext.append(jQuery("<strong/>").text(docs[0].field("title")));
                             } else {
                               confirmtext.append(docs.length + (" " + localization.documents).toLowerCase());
                             }
                             confirmtext.append("?");
                             Confirmation.popup({
                                submit: submit,
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.documents.delete.action,
                                content: confirmtext
                              });
                          }
                }
              ]
    }),
    bottomExtras : function() {
                        var box = $("<div class='table-statuses'/>");
                        var description = function(cssClass,text) {
                            var icon = $("<div class='icon status float-left'></div>").addClass(cssClass);
                            var text = $("<div class='float-left'/>").text(text);
                            return $.merge(icon,text);
                        };
                        box.append(description("draft",localization.archive.documents.statusDescription.draft));
                        box.append(description("cancelled",localization.archive.documents.statusDescription.cancelled));
                        box.append(description("sent",localization.archive.documents.statusDescription.sent));
                        box.append(description("delivered",localization.archive.documents.statusDescription.delivered));
                        box.append(description("read",localization.archive.documents.statusDescription.read));
                        box.append(description("opened",localization.archive.documents.statusDescription.opened));
                        box.append(description("signed",localization.archive.documents.statusDescription.signed));
                        return box;
                    }()
 };

var TemplatesListDefinition = {
    name : "Templatesarchive table",
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Template" },
    sorting: new Sorting({ fields: ["title", "time", "process"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.templates.search}),
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: localization.archive.templates.columns.time, width:"140px", field:"time"}),
        new Cell({name: localization.archive.templates.columns.type, width:"120px", field:"process"}),
        new Cell({name: localization.archive.templates.columns.template, width:"400px", field:"title",  special: "link"}),
        new Cell({name: "", width:"100px", field:"shared", special: "rendered",
                  rendering: function(shared) {
                         var res = jQuery("<p/>")
                         if (shared == "True")
                          return res.text(localization.archive.templates.shared);
                         return res;
                  }})
        ],
    options : [{name : localization.archive.templates.share.action,
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                share: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                            Confirmation.popup({
                                submit: submit,
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.templates.share.head,
                                content: jQuery("<p/>").text(localization.archive.templates.share.body)
                              })
                          }
               },
               {name : localization.archive.templates.delete.action ,
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                remove: "true",
                                                archive: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                             var confirmtext = jQuery("<p/>").append(localization.archive.templates.delete.body + " ");
                             var label = jQuery("<strong/>");
                             if (docs.length == 1) {
                               confirmtext.append(jQuery("<strong/>").text(docs[0].field("title")));
                             } else {
                               confirmtext.append(docs.length + (" " + localization.templates).toLowerCase());
                             }
                             confirmtext.append("?");
                             Confirmation.popup({
                                submit: submit,
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.templates.delete.action,
                                content: confirmtext
                              });
                          }
                }
              ]

    }),
    headerExtras: Button.init({
        color: "green",
        size: "tiny",
        text: localization.archive.templates.createnew,
        name : "doc",
        onClick: function() {
            var popup;
            function doctypebutton(txt,type) {
                return jQuery('<td/>').append(jQuery('<div class="documentTypeBox"/>').append(UploadButton.init({
                    size: "tiny",
                    width : "130",
                    text: txt, //
                    button: jQuery('<a href="#" class="documenticon withUpload"/>').append(jQuery('<div class="documenticonText"/>').append(jQuery("<span class='text'/>").text(txt))),
                    name : "doc",
                    submitOnUpload : true,
                    submit: new Submit({
                        method : "POST",
                        doctype: type,
                        url : "$currentlink$",
                        onSend: function() {LoadingDialog.open();},
                    })
                }).input()));
            }
            var t = jQuery('<tr/>') ;
            t.append(doctypebutton(localization.process.contract.name, "Contract"));
            t.append(doctypebutton(localization.process.offer.name, "Offer"));
            t.append(doctypebutton(localization.process.order.name, "Order"));
            var table = jQuery('<table style="width: 100%"/>').append(jQuery('<tbody/>').append(t));
            popup = Confirmation.popup({
                onAccept: function() { },
                title: localization.archive.templates.createnewtype,
                content: table
            });
            popup.hideAccept();
            return false;
        }
    }).input()
 };

var AttachmentsListDefinition = {
    name : "Attachments Table",
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Attachment" },
    sorting: new Sorting({ fields: ["title", "time", "type"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.attachments.search}),
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: localization.archive.attachments.columns.time, width:"140px", field:"time"}),
        new Cell({name: localization.archive.attachments.columns.attachment, width:"400px", field:"title",  special: "link"}),
        new Cell({name: "", width:"100px", field:"shared", special: "rendered",
                  rendering: function(shared) {
                         var res = jQuery("<p/>")
                         if (shared == "True")
                          return res.text(localization.archive.attachments.shared);
                         return res;
                  }})
        ],
    options : [{name :  localization.archive.attachments.share.action,
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                share: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                            Confirmation.popup({
                                submit: submit,
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.attachments.share.head,
                                content: jQuery("<p/>").text(localization.archive.attachments.share.body)
                              })
                          }
               },
               {name :  localization.archive.attachments.delete.action,
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                remove: "true",
                                                archive: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                             var confirmtext = jQuery("<p/>").append(localization.archive.attachments.delete.body + " ");
                             var label = jQuery("<strong/>");
                             if (docs.length == 1) {
                               confirmtext.append(jQuery("<strong/>").text(docs[0].field("title")));
                             } else {
                               confirmtext.append(docs.length + (" " + localization.attachments).toLowerCase());
                             }
                             confirmtext.append("?");
                             Confirmation.popup({
                                submit: submit,
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.attachments.delete.action,
                                content: confirmtext
                              });
                          }
                }
              ]

    }),
    headerExtras: UploadButton.init({
                    size: "tiny",
                    width : "110",
                    text: localization.archive.attachments.createnew,
                    name : "doc",
                    submitOnUpload : true,
                    submit: new Submit({
                          method : "POST",
                          url : "$currentlink$"
                        })
                    }).input()
 };

var BinListDefinition = {
    name : "Trash table",
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Rubbish" },
    sorting: new Sorting({ fields: ["title", "time", "type"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.bin.search}),
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: localization.archive.bin.columns.time, width:"140px", field:"time"}),
        new Cell({name: localization.archive.bin.columns.type, width:"120px", field:"type"}),
        new Cell({name: localization.archive.bin.columns.title, width:"400px", field:"title",  special: "link"})
        ],
    options : [{name : localization.archive.bin.restore.action,
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                restore: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                            Confirmation.popup({
                                submit: submit,
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.bin.restore.head,
                                content: jQuery("<p/>").text(localization.archive.bin.restore.body)
                              })
                          }
               },
               {name : localization.archive.bin.delete.action,
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                reallydelete: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                              Confirmation.popup({
                                submit: submit,
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.bin.delete.head,
                                content: jQuery("<p/>").text(localization.archive.bin.delete.body)
                              })
                          }
                }
              ]

    })

};

 
 
var ArchiveModel = Backbone.Model.extend({
  defaults : {
      step : "documents"
  },
  documents : function(){
       return this.get("step") == "documents" ;
  },
  templates : function(){
       return this.get("step") == "templates";
  },
  attachments: function() {
       return this.get("step") == "attachments" ;
  },
  bin: function() {
       return this.get("step") == "bin" ;
  },
  goToDocuments : function() {
      this.set({step: "documents"});
  },
  goToTemplates : function() {
      this.set({step: "templates"});
  },
  goToAttachments : function() {
      this.set({step: "attachments"});
  },
  goToBin : function() {
      this.set({step: "bin"});
  }
});

var ArchiveView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        this.render();
    },
    documents : function() {
        return $(KontraList().init(DocumentsListDefinition).view.el);

    },
    templates : function() {
        return $(KontraList().init(TemplatesListDefinition).view.el);

    },
    attachments : function() {
        return $(KontraList().init(AttachmentsListDefinition).view.el);

    },
    bin : function() {
        return $(KontraList().init(BinListDefinition).view.el);

    },
    render: function () {
       var container = $(this.el);
       var archive = this.model;
       var view = this;
       var tabs = new KontraTabs({
        title: "",
        tabs: [
            new Tab({
                active : true,
                name: localization.archive.documents.name,
                elems: [function() {return view.documents();}],
                onActivate : function() {archive.goToDocuments()}
               }),
            new Tab({
                name: localization.archive.templates.name,
                elems: [function() {return view.templates();}],
                onActivate : function() {archive.goToTemplates()}
               }),
            new Tab({
                name: localization.archive.attachments.name,
                elems: [function() {return view.attachments();}],
                onActivate : function() {archive.goToAttachments()}
               }),
            new Tab({
                name: "",
                iconClass : "rubbishbin",
                right: true,
                elems: [function() {return view.bin();}],
                onActivate : function() {archive.goToBin()}
               })
               
       ]});
       container.append(tabs.view.el);
       return this;
    }
});


window.Archive = function(args) {
          var model = new ArchiveModel();
          var view =  new ArchiveView({model : model, el : $("<div/>")});
          return new Object({
              model : function() {return model;},
              view  : function() {return view;}
            });
};

})(window);
