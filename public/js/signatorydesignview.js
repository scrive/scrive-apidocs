/* Signatory view of document
 * Now unified with author and viewer views
 */


(function(window){

window.SignatoryDesignView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('reset', this.render);
        this.model.bind('change:fields', this.render);
        this.model.bind('change:role', this.render);
        this.model.bind('change:csv', this.render);
        this.model.view = this;
        this.render();
    },
    name : function() {
       var signatory = this.model;
       var process = signatory.document().process();
       if (signatory.isCsv())
        return localization.csv.title;
       if (signatory.signs() &&  signatory.author())
        return process.localization().authorsignatoryname + (process.numberedsignatories() ? " " + signatory.signIndex() : "");
       else if(signatory.author())
        return process.localization().authorname;
       else if (signatory.signs())
        return process.localization().signatoryname + (process.numberedsignatories() ? " " + signatory.signIndex() : "");
       else
        return process.localization().nonsignatoryname;
    },
   addCustomFieldButton : function() {
        var signatory = this.model;
        var addFieldButton = $("<a class='addField' href='#'/>");
        addFieldButton.click(function(){
            signatory.addNewCustomField();
            return false;
            });
        return addFieldButton;
   } ,
   refreshRoleSelector: function() {
     var self = this;
     var signatory = self.model;

     if (self.showRoleSelector) {
       self.setRoleIcon.addClass("selected");
       self.setRoleBox = $("<div class='setRoleBox'/>");
       self.container.append(self.setRoleBox);

       var niceClose = function(funcOnChange) {
         self.setRoleIcon.removeClass("selected");
         self.showRoleSelector = false;
         if (self.setRoleBox!= undefined) {
           self.setRoleBox.hide(0, function() {
             if (self.setRoleBox!= undefined) {
               self.setRoleBox.remove();
             }
             if( funcOnChange!=undefined ) {
               funcOnChange();
             }
           });
         }
       }

       var closeButton = $("<a class='modal-close close' href='#'/>");
       closeButton.click(function() {
         niceClose();
         return false;
       });
       self.setRoleBox.append(closeButton);

       var addOption = function(text,checked,funcOnChange) {
         var wrapper = $("<div class='option'/>");
         var checkbox = $("<input class='radio' type='radio'/>");
         var checkboxID = "idf" + Math.random();
         checkbox.attr("id",checkboxID);
         if (checked) {
           checkbox.attr("checked","yes");
         } else {
           checkbox.change(function() {
             niceClose(funcOnChange);
           });
         }
         var label = $("<label class='radiolabel'>").text(text).attr("for", checkboxID);
         wrapper.append(checkbox).append(label);
         self.setRoleBox.append(wrapper);
       };
       addOption(localization.isSigningParty, signatory.signs(),
                 function() { signatory.makeSignatory(); });
       addOption(localization.isNotSigningParty, !signatory.signs(),
                 function() { signatory.makeViewer(); });
     }
     else {
       self.setRoleIcon.removeClass("selected");
       if (self.setRoleBox!= undefined)
         self.setRoleBox.remove();
     }
   },
   setRoleFieldIcon : function() {
       var view = this;
       var signatory = this.model;
       this.setRoleIcon = $("<a class='setRole' href='#'/>");
       this.setRoleIcon.click(function(){
           if (view.showRoleSelector)
               view.showRoleSelector = false;
           else
               view.showRoleSelector = true;
            view.refreshRoleSelector();
            return false;
            });
        return this.setRoleIcon;
   },
   setSignOrderIcon : function() {
       var view = this;
       var signatory = this.model;
       var setSignOrderIcon = $("<a class='setSignOrder' href='#'/>");
       setSignOrderIcon.click(function(){
          signatory.document().view.toggleSignOrder();
            });
        return setSignOrderIcon;
   },
   signOrderSelector : function() {
     var self = this;
     var signatory = this.model;
     var maxSO = signatory.document().maxPossibleSignOrder();
     if (this.signOrderSelectorSelect == undefined) {
        this.signOrderSelectorSelect = $("<select class='selectSignOrder'/>");
        this.signOrderSelectorSelect.change(function(){
          signatory.setSignOrder($(this).val());
        });
        signatory.bind("change:signorder", function() {
          self.signOrderSelectorSelect.val(signatory.signorder());
        });
     }
     if (this.signOrderSelectorSelect.children().size() != maxSO)
     this.signOrderSelectorSelect.children().remove();
     for(var i=1; i<= maxSO; i++){
       var option = $("<option>").attr("value",i).text(i);
       this.signOrderSelectorSelect.append(option);
     }
     this.signOrderSelectorSelect.val(signatory.signorder());
     return this.signOrderSelectorSelect;
   },
   setCsvSignatoryIcon : function() {
       var view = this;
       var signatory = this.model;
       var setCsvSignatoryIcon = $("<a class='setCsvSignatory' href='#'/>");
       setCsvSignatoryIcon.click(function() {
            CsvSignatoryDesignPopup.popup({signatory: signatory});
            return false;
       });
       return setCsvSignatoryIcon;
   },
   placeCheckboxIcon : function() {
       var view = this;
       var signatory = this.model;
       var field = signatory.newCheckbox();
       var placeCheckboxIcon = $("<a class='placeCheckboxIcon' href='#'/>");

       draggebleField(placeCheckboxIcon, field);

       return placeCheckboxIcon;
   },
   placeSignatureIcon : function() {
       var view = this;
       var signatory = this.model;
       var field = signatory.signature();
       var placeSignatureIcon = $("<a class='placeSignatureIcon' href='#'/>");
       field.view = placeSignatureIcon;
       field.view.redborder = function() {placeSignatureIcon.addClass('redborder')};
       field.view.mousedown(function() {placeSignatureIcon.removeClass('redborder')});

       draggebleField(placeSignatureIcon, field);

       return placeSignatureIcon;
   },
   top : function() {
        var top = $("<div class='top'/>");
        var signatory = this.model;
        var document = signatory.document();
        top.append($("<span class='signame'/>").text(this.name().toUpperCase()));
        top.append(this.setRoleFieldIcon());
        if (!signatory.author())
            top.append(this.setCsvSignatoryIcon());
        if (signatory.signs())
            top.append(this.setSignOrderIcon());
        if (signatory.signs())
            top.append(this.placeCheckboxIcon());
        if (signatory.signs())
            top.append(this.placeSignatureIcon());
        top.append(this.addCustomFieldButton());
        if (signatory.signs() && document.view.signOrderVisible())
            top.append(this.signOrderSelector());

        return top;
    },
    postRender : function() {
        this.refreshRoleSelector();
    },
    render: function () {
        var signatory = this.model;
        this.container = $(this.el);
        var view = this;
        this.container.addClass('sigview');
        this.container.children().detach();
        this.container.append(this.top());
        var fields = $("<div class='fields'/>");
        var makeField = function(field) {
                       if (field != undefined)
                        fields.append(new FieldDesignView(
                                        { model : field,
                                          el : $("<div/>")
                                        }).el);
                            };
        makeField(signatory.fstnameField());
        makeField(signatory.sndnameField());
        makeField(signatory.emailField());
        _.each(signatory.fields(),function(field){
            if (field != signatory.fstnameField() &&
                field != signatory.sndnameField() &&
                field != signatory.emailField()   &&
                !field.isSignature() && !field.isCheckbox())
                makeField(field);
        });
        this.container.append(fields);
        this.postRender();
        return this;
    }

});

window.SignatoriesDesignView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'signatoriesList');
        this.model.bind('reset', this.render);
        this.model.bind('change', this.render);
        this.model.bind('change:signatories', this.signatoriesList);

        this.extra = args.extra;
        this.render();
    },
    addRemoveSignatoryBox : function() {
        var document = this.model;
        var view = this;
        var box =  $("<div class='addremovesignatorybox'/>");
        var addLink = $("<a class='addSignatory' href='#'/>").text(localization.addSignatory);
        addLink.click(function(){
            var nsig = document.addSignatory();
            view.showSignatory(nsig);
            return false;
        });
        var removeLink = $("<a class='removeSignatory' href='#'/>").text(localization.removeSignatory);
        removeLink.click(function() {
            document.removeSignatory(view.current);
            return false;

        });
        box.append(addLink).append(removeLink).append(this.extra);
        return box;
    },
    fixCurrent : function() {
      if (this.current == undefined || _.indexOf(this.model.signatories(), this.current) == -1)
          this.current = this.model.signatories()[0];
    },
    signatoriesList : function() {
        var document = this.model;
        var view = this;
        if (view.signatoriesListBox == undefined)
            view.signatoriesListBox =  $("<div class='siglist'/>");
        else
            view.signatoriesListBox.empty();
        var header = $("<a href='#' class='header'/>");
        if (document.view.signOrderVisible())
            header.append($("<span class='float-right'/>").text(localization.signatoryListSignOrder));
        header.append($("<span>").text(localization.signatoryListPartner));
        view.signatoriesListBox.append(header);

        var sigs = document.signatories();
        _.each(document.signatories(),function(sig) {
            var sigline = $("<a href='#' class='signame'/>");
            var setSiglineContent= function() {
                var text = sig.isCsv() ?  localization.csv.title : sig.nameOrEmail();
                if (text == undefined || text == "")
                   text = "(" + localization.noNamePerson + ")";
                var number = sig.signs()? sig.signorder() : "-";
                sigline.empty();
                if (document.view.signOrderVisible())
                  sigline.append($("<span class='float-right'/>").text(number));
                sigline.append($("<span/>").text(text));
            }
            sig.bind('change', function() { setSiglineContent();} );
            setSiglineContent();
            sigline.click(function() {
                view.current= sig;
                view.render();
                return false;
            });
            view.signatoriesListBox.append(sigline);
        });
        return view.signatoriesListBox;
    },
    render: function(){
        this.fixCurrent();
        var box = $(this.el);
        box.children().detach();
        box.addClass('signatoriesbox');
        var document = this.model;
        if (document.signatories().length == 1)
        {
            var s1view = new SignatoryDesignView({model: document.signatories()[0], el: $("<div/>")});
            box.append(s1view.el);
            box.append($("<div class='sigview dummy'/>"));
        }
        else if (document.signatories().length == 2)
        {
            var s1view = new SignatoryDesignView({model: document.signatories()[0], el: $("<div/>")});
            var s2view = new SignatoryDesignView({model: document.signatories()[1], el: $("<div/>")});
            box.append(s1view.el);
            box.append(s2view.el);

        }
        else
        {
           box.append(this.signatoriesList());
           var sview = new SignatoryDesignView({model: this.current, el: $("<div/>")});
           box.append(sview.el);

        }
        box.append(this.addRemoveSignatoryBox());
        return this;
    },
    showSignatory : function(sig) {
        this.current= sig;
        this.render();
        return true;
    }
});


})(window);
