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
        newDocumentBox: function() {
          var button = Button.init({
                width: 145,
                size : "small",
                text: localization.uploadView.newProcessFromFile,
                color: "green",
                onClick: function() {
                    // blocking
                    if(uploadBlocking && uploadBlocking.shouldBlockDocs(1)) {
                       uploadBlocking.createPopup();
                       return false;
                     }

                 new Submit({
                     
                    method : "POST",
                    url : "/api/frontend/createfromfile",
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
          var uploadbuttoncontainer = $("<div class='signStepsButtonContainer s-upload-document'></div>").append(button.input());
          return $("<div/>").append(uploadbuttoncontainer);
        },
        templateBox : function() {
          var wiz = this.model.wizard();
          var button = Button.init({
            color : 'green',
            size : 'small' ,
            width : 145,
            text : localization.uploadView.chooseTemplate,
            onClick : function() {
                // blocking
                if(uploadBlocking && uploadBlocking.shouldBlockDocs(1)) {
                    uploadBlocking.createPopup();
                    return false;
                }

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
            d.append(this.newDocumentBox());
            d.append(this.templateBox());
            $(view.el).append(d);
            d = $(div);
            d.append($(header).text(localization.uploadView.createNewTemplate));
            d.append(this.newTemplateBox());
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
                                                      window.location.href = "/d/"+d.id;
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
