(function(window){

    window.ChangeFileUploadView = Backbone.View.extend({
        tagName: "tr",
        initialize: function() {
            this.model.view = this;
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
        },
        render: function() {
          var view = this;

          $(view.el).children().detach();
          var model = view.model;
          var wiz = model.wizard();

          var textview = new UploadTextView({text:localization.docupload.uploadinfo});
          $(view.el).append(textview.el);

          var url = "/api/mainfile/" + KontraDesignDocument.model.id;
          var upbutton = UploadButton.init({
            name: "file",
            width: 130,
            text: localization.uploadButton,
            submitOnUpload: true,
            onClick : function () {
              LoadingDialog.open();
            },
            onError: function() {
              wiz.trigger('change');
              LoadingDialog.close();
            },
            submit: new Submit({
              method : "POST",
              url : url,
              ajax: true,
              beforeSend: function() {
              },
              onSend: function() {
                LoadingDialog.open();
              },
              ajaxerror: function(d,a){
                if(a === 'parsererror') // file too large
                  FlashMessages.add({content: localization.fileTooLarge, color: "red"});
                else
                  FlashMessages.add({content: localization.couldNotUpload, color: "red"});
                LoadingDialog.close();
                wiz.trigger('change');
              },
              ajaxsuccess: function() {
                SessionStorage.set(KontraDesignDocument.model.documentid(), "step", "2");
                window.location.reload();
              }
            })
          });

          var upbox = new UploadBoxModel({
            headertext: localization.docupload.choosefile,
            subtext: localization.onlyPDF,
            button: upbutton
          });

          var upboxview = new UploadBoxView({model: upbox});
          $(view.el).append(upboxview.el);

          var tempbutton = Button.init({
            color: "green",
            size: "small",
            text: localization.docupload.choosetemplate,
            width: "130",
            onClick: function () {
              wiz.nextStep();
              return false;
            }
          });

          var tempbox = new UploadBoxModel({
            headertext: localization.docupload.choosetemplate,
            subtext: localization.docupload.templatesaved,
            button: tempbutton
          });

          var tempboxview = new UploadBoxView({model: tempbox});

          $(view.el).append(tempboxview.el);
        }
    });

    window.ChangeFileTemplateView = Backbone.View.extend({
        tagName: "tr",
        initialize: function() {
            this.model.view = this;
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            var model = this.model;
        },
        extras: function() {
          var model = this.model;
          return $("<div>").addClass("float-left basicinfo")
            .append($("<p>").text(localization.infoSelectTemplate))
            .append($("<br>"))
            .append($("<p>").append($("<a href='#' />").addClass("jsback1 backicon float-left boo").html(" &nbsp;"))
                    .click(function() {
                      if(model.wizard())
                        model.wizard().previousStep();
                      return false;
                    })
                    .append(" &nbsp;" + localization.goBack));
        },
        render: function() {
            var view = this;
            var el = $(view.el);
            var model = view.model;
            var wiz = model.wizard();

            el.children().detach();

            var documentsTable = KontraList().init({
                name : "Templates table",
                schema: new Schema({
                    url: "/docs",
                    extraParams : { documentType : "Template|" + KontraDesignDocument.model.process().name() },
                    sorting: new Sorting({ fields: ["title"]}),
                    paging: new Paging({}),
                    filtering: new Filtering({text: "", infotext: localization.searchTemplate}),
                    cells : [
                        new Cell({name: localization.sortTemplate,
                                  width:"400px",
                                  field:"title",
                                  special: "rendered",
                                  rendering : function(title, _mainrow, listobject) {
                                      var link = jQuery("<a />").text(title);
                                      link.click(function(){
                                          new Submit({
                                            method : "POST",
                                            ajax: true,
                                            url: "/api/mainfile/" + KontraDesignDocument.model.id,
                                            template: listobject.field("id"),
                                            ajaxsuccess: function() {
                                              SessionStorage.set(KontraDesignDocument.model.documentid(), "step", "2");
                                              window.location.reload();
                                            }
                                          }).send();
                                          return false;
                                      });
                                      return link;
                                  }
                                 })
                    ]
                }),
                headerExtras : view.extras()

            });
            documentsTable.view.render();


            el.append($("<td>").addClass("templateslist").append(documentsTable.view.el));


        }
    });
})(window);
