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
            this.text = $("#jschooseprocesstext").html();
            this.choose = $("#jschooseprocess").html();
        },
        render: function() {
            var view = this;
            $(view.el).children().detach();
            var model = view.model;
            var wiz = model.wizard();
            $(view.el).append(this.text);
            $.each(model.get('processes'), function(i, v) {
                var n = $(view.choose);
                var a = n.find("a");
                a.click(function(){
                    wiz.set({process: v});
                    wiz.nextStep();
                    return false;
                });
                a.text(v.localname);
                $(view.el).append(n);
            });
        }
    });

    window.UploadProcessView = Backbone.View.extend({
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
            if(t.find("a.jswizardback").length > 0)
              t.find("a.jswizardback").click(function() {
                wiz.previousStep();
                return false;
              });
            if(t.find(".jsprompt").length > 0)
              t.find(".jsprompt").text(wiz.get('process').prompt);
            $(view.el).append(t);
            var url = "/api/document";
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
                  json: '{"type" : ' + (wiz.get('process')?wiz.get('process').signable:"1") + '}',
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
                            window.location.href = d.designurl;
                        }
                    }
                })
            });
            var up = $(view.upload);
            up.find(".signStepsBodyUploadBox .signStepsButtonContainer").append(upbutton.input());
            $(view.el).append(up);
            var temps = $(view.template);
            temps.find("a").click(function(event) {
                event.preventDefault();
                wiz.nextStep();
                return false;
            });
            $(view.el).append(temps);
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
                    filtering: new Filtering({text: "", infotext: localization.searchTemplate}),
                    cells : [
                        new Cell({name: localization.sortTemplate,
                                  width:"400px",
                                  field:"title",
                                  special: "rendered",
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
                    .append($("<br>"))
                    .append($("<p>").append($("<a href='#' />").addClass("jsback1 backicon float-left boo").html(" &nbsp;"))
                            .append(" &nbsp;" + localization.goBack))

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
                     signable: "1",
                     prompt: localization.process.contract.uploadprompt
                    },
                    {name: "Offer",
                     localname: localization.process.offer.name,
                     signable: "3",
                     prompt: localization.process.offer.uploadprompt
                    },
                    {name: "Order",
                     localname: localization.process.order.name,
                     signable: "5",
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
