/* The process for uploading a new document.
   Written by Eric Normand
 */
(function(window){
    window.SelectProcessView = Backbone.View.extend({
        tagName: "tr",
        initialize: function() {
            this.model.view = this;
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);   
        },
        text : function() {
           var textp = $("<p class='text' style='padding-top:40px'/>").text(localization.uploadView.signStepsBodyUpload);
           var textdiv = $("<div class='signStepsBodyUploadBox' style='border:0px;text-align:left;width: 250px;font-weight:bold'/>").append(textp);
           return $("<td/>").append(textdiv);
        },
        choose : function(i, v) {
           var wiz = this.model.wizard();
           var a = $("<a class='documenticon' href='#'/>").append($("<span class='text'/>").text(v.localname));
           a.click(function(){
                    wiz.set({process: v});
                    wiz.nextStep();
                    return false;
           });
           return $("<td/>").append($("<div class='documentTypeBox'/>").append(a));
        },
        render: function() {
            var view = this;
            $(view.el).children().detach();
            var model = view.model;
            var wiz = model.wizard();
            $(view.el).append(this.text());
            $.each(model.get('processes'), function(i, v) {
                $(view.el).append(view.choose(i,v));
            });
        }
    });

    window.UploadProcessView = Backbone.View.extend({
        tagName: "tr",
        initialize: function() {
            this.model.view = this;
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.upload = $("#jsuploadbutton").html();
            this.template = $("#jschoosetemplate").html();
        },
        text : function() {
            var wiz = this.model.wizard();
            var prompt = $("<p class='text jsprompt' style='padding-top:30px'></p>").text(wiz.get('process').prompt);
            var a = $("<a class='backicon float-left jswizardback' href='#'></a>");
            a.click(function() {
                wiz.previousStep();
                return false;
            });
            var p = $("<p style='padding-top:25px;'/>").append(a).append($("<span class='float-left upload-back-text'/>").text(localization.uploadView.back));
            var div = $("<div class='signStepsBodyUploadBox' style='border:0px;text-align:left;width: 250px;font-weight:bold'/>").append(prompt).append(p);
            return $("<td/>").append(div);
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
                    url : "/api/document",
                    ajax: true,
                    json: JSON.stringify({"type" : (wiz.get('process') ? wiz.get('process').signable : 1)}),
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
                        if (d != undefined && d.designurl != undefined) {
                            window.location.href = d.designurl;
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
        templateBox : function() {
          var wiz = this.model.wizard();
          var header = $("<span class='header'/>").text(localization.uploadView.chooseTemplate);
          var subheader = $("<span class='text'/>").text(localization.uploadView.savedTemplate);
          var button = Button.init({
            color : 'green',
            size : 'small' ,
            width : 134,
            text : localization.uploadView.chooseTemplate,
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
        render: function() {
            var view = this;
            $(view.el).children().detach();
            var model = view.model;
            var wiz = model.wizard();
            $(view.el).append(this.text());
            $(view.el).append(this.uploadBox());
            $(view.el).append(this.templateBox());
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
                    extraParams : { documentType : "Template|" + wiz.get('process').name },
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
                                              url: "/t",
                                              template: listobject.field("id")
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

        var sp = new WizardStep;

        sp.set({processes:
                [
                    {name: "Contract",
                     localname: localization.process.contract.name,
                     signable: 1,
                     prompt: localization.process.contract.uploadprompt
                    },
                    {name: "Offer",
                     localname: localization.process.offer.name,
                     signable: 3,
                     prompt: localization.process.offer.uploadprompt
                    },
                    {name: "Order",
                     localname: localization.process.order.name,
                     signable: 5,
                     prompt: localization.process.order.uploadprompt
                    }
                ]
               },
               {silent: true});

        var spview = new SelectProcessView({model: sp});

        var up = new WizardStep;
        var upview = new UploadProcessView({model: up});

        var tmp = new WizardStep;
        var tmpview = new SelectTemplateView({model: tmp});
        wizard.addStep(sp);
        wizard.addStep(up);
        wizard.addStep(tmp);

        wizardview.render();

    };
})(window);
