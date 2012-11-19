
(function(window){

var  CreateFromTemplateModel = Backbone.Model.extend({
  defaults: {
  },
  list : function() {
    if (this.get("list") == undefined)
      this.set({"list" : new KontraList({
        name : "Templatesarchive table",
        loadOnInit : false,
        schema: new Schema({
              url: "/docs",
              extraParams : { documentType : "Template" },
              sorting: new Sorting({ fields: ["title", "time", "process"]}),
              paging: new Paging({}),
              textfiltering: new TextFiltering({text: "", infotext: localization.archive.templates.search}),
              selectfiltering : [ new SelectFiltering({
                                      name: "process",
                                      textWidth : "100px",
                                      options: [
                                                  {name: localization.filterByProcess.showAllProcesses, value: ""},
                                                  {name: localization.filterByProcess.showContractsOnly, value: "contract"},
                                                  {name: localization.filterByProcess.showOffersOnly,    value: "offer"},
                                                  {name: localization.filterByProcess.showOrdersOnly,    value: "order"}
                                                ]})
                      ],
              cells : [
                  new Cell({name: localization.archive.templates.columns.shared, width:"52px", field:"shared", special: "rendered",
                            rendering: function(shared) {
                                  return $("<div/>").addClass((shared == "True") ? "sharedIcon" : "notSharedIcon");
                            }}),

                  new Cell({name: localization.archive.templates.columns.time, width:"140px", field:"time"}),
                  new Cell({name: localization.archive.templates.columns.verificationMethod, width:"100px", field:"id",  special: "rendered",
                            rendering: function(value,_idx,model) {
                                   var res= $("<div/>");
                                    if (model.field("authentication") == "eleg")
                                        res.text(localization.eleg)
                                    else if (model.field("delivery") == "pad")
                                        res.text(localization.pad.delivery)
                                    else
                                        res.text(localization.email)
                                    return res;
                            }}),
                  new Cell({name: localization.archive.templates.columns.template, width:"450px", field:"title",
                            rendering : function(title, _mainrow, listobject) {
                                      var link = jQuery("<a/>").text(title);
                                      link.click(function(){
                                          new Submit({
                                              method : "POST",
                                              url: "/api/frontend/createfromtemplate/" +  listobject.field("id"),
                                              ajax: true,
                                              expectedType : "text",
                                              onSend: function() {
                                                      LoadingDialog.open();
                                              },
                                              ajaxerror: function(d,a){
                                                      LoadingDialog.close();
                                              },
                                              ajaxsuccess: function(d) {
                                                 try {
                                                        window.location.href = "/d/"+JSON.parse(d).id;
                                                  } catch(e) {
                                                      LoadingDialog.close();
                                                  }
                                              }
                                          }).send();
                                          return false;
                                      });
                                      return link;
                                  }}),
                  new Cell({name: localization.archive.templates.columns.type, width:"80px", field:"process",
                        rendering: function(value, _idx, _model) {
                                var txt = "";
                                if( localization.process[value] !== undefined ) {
                                    txt = localization.process[value].shortName;
                                }
                                return jQuery("<span />").text(txt);
                              }
                  })
                  ]
              })
        })
      });

    return this.get("list");  
  }
});

var CreateFromTemplateView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.render();
  },
  header : function() {
    var box = $("<div class='header'/>");
    box.append($("<div class='headline'>").text(localization.createFromTemplateDescription));
    return box;
  },
  render: function() {
     var model = this.model
     var container = $(this.el);
     container.append(this.header());
     model.list().recall();
     container.append(model.list().el());
     return this;
  }
});

window.CreateFromTemplate = function(args) {
    var model = new CreateFromTemplateModel(args);
    var view = new CreateFromTemplateView({ model: model, el : $("<div class='tab-viewer create-from-template'>")});
    return {
      el: function() { return $(view.el); }
    };
};


})(window);
