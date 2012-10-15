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
          var upbutton = UploadButton.init({
                name: "file",
                width: 125,
                text: localization.uploadView.newProcessFromFile,
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
                    url : "/api/frontend/createfromfile",
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
          return $("<div/>").append(uploadbuttoncontainer);
        },
        templateBox : function() {
          var wiz = this.model.wizard();
          var button = Button.init({
            color : 'green',
            size : 'small' ,
            width : 125,
            text : localization.uploadView.chooseTemplate,
            onClick : function() {
                wiz.nextStep();
                return false;
            }
          });
          var buttonbox = $("<div class='signStepsButtonContainer' style='height:34px'/>");
          buttonbox.append(button.input().addClass("selectTemplateButton"));
          return $("<div/>").append(buttonbox);
        },
        newTemplateBox: function() {
          var wiz = this.model.wizard();
          var upbutton = UploadButton.init({
                name: "file",
                width: 145,
                text: localization.uploadView.uploadNewTemplate,
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
          var uploadbuttoncontainer = $("<div class='signStepsButtonContainer' style='width: 145px;'></div>").append(upbutton.input());
          return $("<div/>").append(uploadbuttoncontainer);
        },
        newEmptyTemplateBox: function() {
          var wiz = this.model.wizard();
          var button = Button.init({
                width: 145,
                size : "small",
                text: "New Template",
                color: "green",
                onClick: function() {
                    new Submit({
                        method : "POST",
                        url : "/api/createfromfile",
                        template : "YES",
                        ajax: true,
                        expectedType: 'json',
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
                }
          });
          var container = $("<div class='signStepsButtonContainer' style='width: 145px;'></div>").append(button.input());
          return $("<div/>").append(container);
        },
        avtal24Box: function() {
            var wiz = this.model.wizard();
            var label = $("<div class='label' style='text-align: center; width: 113px;'/>").text(localization.uploadView.buyAvtal24Template);
            var link = $("<a href='https://avtal24.se' class='green btn-small'/>").append("<div class='left'/>").append(label).append("<div class='right'/>");
            var container = $("<div class='signStepsButtonContainer' style='width: 145px;'></div>").append(link);
            return $("<div/>").append(container);
        },
        render: function() {
            var view = this;
            $(view.el).children().detach();
            var model = view.model;
            var wiz = model.wizard();
            var div = "<div class='signStepsBodyUploadBox' style='height: 120px; width: 280px;'/>";
            var header = "<span class='header' style='font-variant: small-caps;'/>";
            var d = $(div);
            $(view.el).append($("<div style='float: left; height: 1px; width: 135px;'/>"));
            d.append($(header).text(localization.uploadView.startNewProcess));
            d.append(this.uploadBox());
            d.append(this.templateBox());
            $(view.el).append(d);
            d = $(div);
            d.append($(header).text(localization.uploadView.createNewTemplate));
            d.append(this.newEmptyTemplateBox());
            d.append(this.avtal24Box());
            $(view.el).append(d);
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
        sendConfirmationForPadDelivery : function(document) {
          var view = this;
          var box = $("<div>");
          var padDesignViewUtil = new PadDesignViewUtils({document : document});
          box.append($("<div/>").text(localization.pad.maybeYouWantToSendDirectly));
          var checkbox = $("<input type='checkbox' autocomplete='false'/>");
          var updatepadDesignViewUtil = function() {
              if (checkbox.is(":checked"))
                padDesignViewUtil.el().show();
              else
                padDesignViewUtil.el().hide();
            };
          checkbox.change(updatepadDesignViewUtil);
          updatepadDesignViewUtil();
          var label = $("<span class='label'/>").text(localization.pad.sendDirectly);
          box.append($("<div class='padoptions'/>").append($("<div class='padoption'/>").append(checkbox).append(label)));
          box.append(padDesignViewUtil.el());
          var acceptButton = Button.init({
                                    size: "small",
                                    color : "green",
                                    text : localization.pad.goToDesignView,
                                    onClick : function() {
                                        if (alreadyClicked(this))
                                          return;
                                        LoadingDialog.open(localization.designview.messages.sendingDocument);
                                        var link = "/d/" + document.documentid();
                                        if (!checkbox.is(":checked"))
                                          window.location = link;
                                        else
                                          document.sendByAuthor().sendAjax(function(resp) {
                                                      padDesignViewUtil.postSendAction(link);
                                          });
                                    }
                                  });
          var updatepadAcceptButtonText = function() {
              if (checkbox.is(":checked"))
                acceptButton.setText(document.process().sendbuttontext());
              else
                acceptButton.setText(localization.pad.goToDesignView);
            };
          checkbox.change(updatepadAcceptButtonText);
          Confirmation.popup({
                  title : localization.pad.documentCreatedFromTemplate,
                  acceptButton : acceptButton.input(),
                  rejectText: localization.cancel,
                  content  : box
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
                                              url: "/api/frontend/createfromtemplate/" +  listobject.field("id"),
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
                                                          var doc = new Document({id : d.id});
                                                          doc.bind('change:ready',function() {
                                                            if (!doc.ready()) return;
                                                            if (doc.padDelivery() && doc.readyToBeSend())
                                                              {
                                                                view.sendConfirmationForPadDelivery(doc);
                                                              }
                                                            else
                                                              window.location.href = "/d/"+d.id;
                                                              
                                                          });
                                                          doc.recall();
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
        window.UploadWizardView = wizardview;

        var up = new WizardStep;
        var upview = new UploadProcessView({model: up});

        var tmp = new WizardStep;
        var tmpview = new SelectTemplateView({model: tmp});

        wizard.addStep(up);
        wizard.addStep(tmp);

        wizardview.render();
        return {
          el : function() {return wizardview.el;}
        };

    };
})(window);
