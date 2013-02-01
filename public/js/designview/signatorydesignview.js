/* Signatory view of document
 * Now unified with author and viewer views
 *
 * Instrumented with Mixpanel
 */


(function(window){

var SignatoryDesignModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
  },
  signatory : function() {
     return this.get("signatory");
  },
  documentdesignview :function() {
     return this.get("documentdesignview");
  }
});
  
var SignatoryDesignView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.signatory().bind('reset', this.render);
        this.model.signatory().bind('change:fields', this.render);
        this.model.signatory().bind('change:role', this.render);
        this.model.signatory().bind('change:csv', this.render);
        this.render();
    },
   addCustomFieldButton : function() {
        var self = this;
        var signatory = this.model.signatory();
        var addFieldButton = $("<a class='addField' href='#'/>");
        addFieldButton.click(function(){
            var fields = self.fields;
            var currentPosition = fields.scrollTop();
            signatory.addNewCustomField();

            // this should work, but using old reference doesn't scroll at all
            // but getting new reference does:/
            // fields.scrollTop(currentPosition);
            $('div.fields').scrollTop(currentPosition);

            mixpanel.track('Add custom field',
                           {'Signatory Index': signatory.signIndex(),
                            'Author?': signatory.author()});
            return false;
            });
        return addFieldButton;
   } ,
   refreshRoleSelector: function() {
     var self = this;
     var signatory = self.model.signatory();

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

       var closeButton = $("<a class='close' href='#'/>");
       closeButton.click(function() {
         niceClose();
           mixpanel.track('Close role selector',
                          {'Signatory Index': signatory.signIndex(),
                           'Author?': signatory.author(),
                           Button: 'X'});
         return false;
       });
       self.setRoleBox.append(closeButton);

       var addOption = function(text,checked,funcOnChange) {
         var wrapper = $("<label class='option'/>");
         var checkbox = $("<input class='radio' type='radio'/>");
         if (checked) {
           checkbox.attr("checked","yes");
         } else {
           checkbox.change(function() {
             niceClose(funcOnChange);
           });
         }
         var label = $("<span class='radiolabel'/>").text(text);
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
       var signatory = this.model.signatory();
       this.setRoleIcon = $("<a class='setRole' href='#'/>");
       this.setRoleIcon.click(function(){
           if (view.showRoleSelector) {
               view.showRoleSelector = false;
                mixpanel.track('Close role selector',
                          {'Signatory Index': signatory.signIndex(),
                           'Author?': signatory.author(),
                           Button: 'icon'});
           } else {
               view.showRoleSelector = true;
               mixpanel.track('Show role selector',
                              {'Signatory Index': signatory.signIndex(),
                               'Author?': signatory.author()});
           }
            view.refreshRoleSelector();
            return false;
            });
        return this.setRoleIcon;
   },
   setSignOrderIcon : function() {
       var view = this;
       var model = this.model;
       var signatory = model.signatory();
       var setSignOrderIcon = $("<a class='setSignOrder' href='#'/>");
       setSignOrderIcon.click(function(){
           mixpanel.track('Toggle sign order',
                          {'Signatory Index': signatory.signIndex(),
                           'Author?': signatory.author()});
           model.documentdesignview().toogleSignOrder();
          return false;
       });
        return setSignOrderIcon;
   },
   signOrderSelector : function() {
     var self = this;
     var signatory = this.model.signatory();
     var maxSO = signatory.document().maxPossibleSignOrder();
     if (this.signOrderSelectorSelect == undefined) {
        this.signOrderSelectorSelect = $("<select class='selectSignOrder'/>");
        this.signOrderSelectorSelect.change(function(){
            var val = $(this).val();
            mixpanel.track('Change sign order',
                           {'Signatory Index': signatory.signIndex(),
                            'Author?': signatory.author(),
                            'Value': val});
          signatory.setSignOrder(val);
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
       var signatory = this.model.signatory();
       var setCsvSignatoryIcon = $("<a class='setCsvSignatory' href='#'/>");
       setCsvSignatoryIcon.click(function() {
           mixpanel.track('Click CSV icon',
                          {'Signatory Index': signatory.signIndex(),
                           'Author?': signatory.author()});
            CsvSignatoryDesignPopup.popup({signatory: signatory});
            return false;
       });
       return setCsvSignatoryIcon;
   },
   placeCheckboxIcon : function() {
       var view = this;
       var signatory = this.model.signatory();
       var field = signatory.newCheckbox();
       var placeCheckboxIcon = $("<a class='placeCheckboxIcon' href='#'/>");
       draggebleField(placeCheckboxIcon, field);
       return placeCheckboxIcon;
   },
   placeSignatureIcon : function() {
       var view = this;
       var signatory = this.model.signatory();
       var field = signatory.newSignature();
       var placeSignatureIcon = $("<a class='placeSignatureIcon' href='#'/>");
       field.view = placeSignatureIcon;
       field.view.redborder = function() {placeSignatureIcon.addClass('redborder')};
       field.view.mousedown(function() {placeSignatureIcon.removeClass('redborder')});
       draggebleField(placeSignatureIcon, field);
       return placeSignatureIcon;
   },
   top : function() {
        var top = $("<div class='top'/>");
        var signatory = this.model.signatory();
        top.append($("<h5 class='signame'/>").text(signatory.nameInDocument()));
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
        if (signatory.signs() && this.model.documentdesignview().signOrderVisible())
            top.append(this.signOrderSelector());

        return top;
    },
    postRender : function() {
        this.refreshRoleSelector();
    },
    render: function () {
        var signatory = this.model.signatory();
        this.container = $(this.el);
        var view = this;
        this.container.addClass('sigview');
        this.container.children().detach();
        this.container.append(this.top());
        var fields = $("<div class='fields'/>");
        view.fields = fields;
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

window.SignatoryDesign = function(args) {
          var model = new SignatoryDesignModel(args);
          var view =  new SignatoryDesignView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};
    
};


})(window);
