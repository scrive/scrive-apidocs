(function(window){

    window.ChangeFileUploadView = Backbone.View.extend({
        tagName: "tr",
        initialize: function() {
            this.model.view = this;
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.text = $("#jsuploadtext").html();
            this.upload = $("#jsuploadbutton").html();
            this.template = $("#jschoosetemplate").html();
        },
        render: function() {
            var view = this;
            $(view.el).children().detach();
            var model = view.model;
            var wiz = model.wizard();
            var t = $(view.text);
            $(view.el).append(t);
            var url = "/api/mainfile/" + KontraDesignDocument.model.id;
            var upbutton = UploadButton.init({
                name: "file",
                width: 125,
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
            var up = $(view.upload);
            up.find(".signStepsBodyUploadBox .signStepsButtonContainer").append(upbutton.input());
            $(view.el).append(up);
            var temps = $(view.template);
            temps.find("a").click(function() {
                wiz.nextStep();
                return false;
            });
            $(view.el).append(temps);
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
