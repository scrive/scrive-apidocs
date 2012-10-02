/* The process for uploading a new document.
   Written by Eric Normand
 */
(function(window){
    window.UploadProcessView = Backbone.View.extend({
        tagName: "tr",
        initialize: function() {
            this.model.view = this;
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.upload = $("#jsuploadbutton").html();
            this.template = $("#jschoosetemplate").html();
        },
        uploadBox: function() {
          var wiz = this.model.wizard();
          var header = $("<span class='header'/>").text(localization.uploadView.chooseFile);
          var subheader = $("<span class='text'/>").text(localization.onlyPDF);
          var upbutton = UploadButton.init({
                name: "file",
                width: 125,
                text: localization.uploadButton,
                submitOnUpload: true,
                type: "",
                onClick : function () {
                    LoadingDialog.open();
                },
                onError: function() {
                    wiz.trigger('change');
                    LoadingDialog.close();
                },
                submit: new Submit({
                    method : "POST",
                    url : "/api/createfromfile",
                    ajax: true,
                    expectedType: 'json',
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
                    ajaxsuccess: function(d) {
                        if (d != undefined && d.id != undefined) {
                            window.location.href = "/d/"+d.id;
                        }
                        else {
                             FlashMessages.add({content: localization.couldNotUpload, color: "red"});
                             LoadingDialog.close();
                             wiz.trigger('change');
                        }
                    }
                })
            });
          var uploadbuttoncontainer = $("<div class='signStepsButtonContainer s-upload-document'></div>").append(upbutton.input());
          var div = $("<div class='signStepsBodyUploadBox'/>").append(header).append("<BR/>").append(subheader).append("<BR/>").append(uploadbuttoncontainer);    
          return $("<td class='jsformbox'>").append(div); 
        },
        templateBox : function() {
          var wiz = this.model.wizard();
          var header = $("<span class='header'/>").text(localization.uploadView.chooseTemplate);
          var subheader = $("<span class='text'/>").text(localization.uploadView.savedTemplate);
          var button = Button.init({
            color : 'green',
            size : 'small' ,
            width : 134,
            text : localization.uploadView.chooseTemplateButton,
            onClick : function() {
                wiz.nextStep();
                return false;
            }
          });
          var buttonbox = $("<div class='signStepsButtonContainer' style='height:34px'/>");
          buttonbox.append(button.input().addClass("selectTemplateButton"));
          var div = $("<div class='signStepsBodyUploadBox'/>").append(header).append("<BR/>").append(subheader).append("<BR/>").append(buttonbox);
          return $("<td/>").append(div);
        },
        newTemplateBox: function() {
          var wiz = this.model.wizard();
          var header = $("<span class='header'/>").text(localization.uploadView.createNewTemplate);
          var subheader = $("<span class='text'/>").text(localization.onlyPDF);
          var upbutton = UploadButton.init({
                name: "file",
                width: 125,
                text: localization.uploadButton,
                submitOnUpload: true,
                type: "",
                onClick : function () {
                    LoadingDialog.open();
                },
                onError: function() {
                    wiz.trigger('change');
                    LoadingDialog.close();
                },
                submit: new Submit({
                    method : "POST",
                    url : "/api/createfromfile",
                    ajax: true,
                    template : "YES",
                    expectedType: 'json',
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
                    ajaxsuccess: function(d) {
                        if (d != undefined && d.id != undefined) {
                            window.location.href = "/d/"+d.id;
                        }
                        else {
                             FlashMessages.add({content: localization.couldNotUpload, color: "red"});
                             LoadingDialog.close();
                             wiz.trigger('change');
                        }
                    }
                })
            });
          var uploadbuttoncontainer = $("<div class='signStepsButtonContainer'></div>").append(upbutton.input());
          var div = $("<div class='signStepsBodyUploadBox'/>").append(header).append("<BR/>").append(subheader).append("<BR/>").append(uploadbuttoncontainer);
          return $("<td class='jsformbox'>").append(div);
        },
        render: function() {
            var view = this;
            $(view.el).children().detach();
            var model = view.model;
            var wiz = model.wizard();
            $(view.el).append(this.uploadBox());
            $(view.el).append(this.templateBox());
            $(view.el).append(this.newTemplateBox());
        }
    });

    window.SelectTemplateView = Backbone.View.extend({
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
                    extraParams : { documentType : "Template" },
                    sorting: new Sorting({ fields: ["title"]}),
                    paging: new Paging({}),
                    filtering: new TextFiltering({text: "", infotext: localization.searchTemplate}),
                    cells : [
                        new Cell({name: localization.sortTemplate,
                                  width:"400px",
                                  field:"title",
                                  rendering : function(title, _mainrow, listobject) {
                                      var link = jQuery("<a/>").text(title);
                                      link.click(function(){
                                          new Submit({
                                              method : "POST",
                                              url: "/api/createfromtemplate/" +  listobject.field("id"),
                                              ajax: true,
                                              expectedType: 'json',
                                              onSend: function() {
                                                      LoadingDialog.open();
                                              },
                                              ajaxerror: function(d,a){
                                                      LoadingDialog.close();
                                                      wiz.trigger('change');
                                              },
                                              ajaxsuccess: function(d) {
                                                      if (d != undefined && d.id != undefined) {
                                                          window.location.href = "/d/"+d.id;
                                                      }
                                                      else {
                                                          LoadingDialog.close();
                                                          wiz.trigger('change');
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
                    .append($("<p style='padding-top: 20px;'>").append($("<a class='jsback1 backicon float-left boo' href='#'/>"))
                            .append($("<span class='upload-back-text float-left'/>").text(localization.uploadView.back)))
            });
            documentsTable.view.render();

            el.children().detach();
            el.append($("<td>").addClass("templateslist").append(documentsTable.view.el));


        }
    });

    window.UploadProcessMain = function(){
        var wizard = new Wizard;
        var wizardview = new WizardView({model: wizard});
        wizardview.el = $(".signStepsBody table tbody");
        window.UploadWizardView = wizardview;

        var up = new WizardStep;
        var upview = new UploadProcessView({model: up});

        var tmp = new WizardStep;
        var tmpview = new SelectTemplateView({model: tmp});

        wizard.addStep(up);
        wizard.addStep(tmp);

        wizardview.render();

    };
})(window);
