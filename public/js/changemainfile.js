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
            var url = "/api/document/" + KontraDesignDocument.model.id;
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
                    expectedType: 'json',
                    beforeSend: function() {
                        console.log("first");
                    },
                    onSend: function() {
                        console.log("here");
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
                    ajaxsuccess: function(d) {
                      console.log("there");
                        if (d) {
                          SessionStorage.set(KontraDesignDocument.model.documentid(), "step", "2");
                            window.location.href = d.designurl;
                        }
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
            // I had to put this callback here because
            // it wasn't working in-place
            //it's a hack but something must be interfering
            // with the click event
            $("a.jsback1").live('click', function() {
                if(model.wizard())
                    model.wizard().previousStep();
                return false;
            });
        },
        render: function() {
            var view = this;
            var el = $(view.el);
            var model = view.model;
            var wiz = model.wizard();
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
                                            expectedType: 'json',
                                            url: "/api/document/" + KontraDesignDocument.model.id,
                                            template: listobject.field("id"),
                                            ajaxsuccess: function(d) {
                                              console.log("there");
                      
                                              if (d) {
                                                SessionStorage.set(KontraDesignDocument.model.documentid(), "step", "2");
                                                window.location.href = d.designurl;
                                              }

                                            }
                                          }).send();
                                          return false;
                                      });
                                      return link;
                                  }
                                 })
                    ]
                }),
                headerExtras : $("<div>").addClass("float-left basicinfo")
                    .append($("<p>").text(localization.infoSelectTemplate))
                    .append($("<br>"))
                    .append($("<p>").append($("<a href='#' />").addClass("jsback1 backicon float-left boo").html(" &nbsp;"))
                            .append(" &nbsp;" + localization.goBack))

            });
            documentsTable.view.render();

            el.children().detach();
            el.append($("<td>").addClass("templateslist").append(documentsTable.view.el));


        }
    });
})(window);
