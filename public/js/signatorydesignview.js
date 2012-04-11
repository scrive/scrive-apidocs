/* Signatory view of document
 * Now unified with author and viewer views
 */


(function(window){

window.SignatoryDesignViewBasic = Backbone.View.extend({
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
        return process.authorsignatoryname() + (process.numberedsignatories() ? " " + signatory.signIndex() : "");
       else if(signatory.author())
        return process.authorname();
       else if (signatory.signs())
        return process.signatoryname() + (process.numberedsignatories() ? " " + signatory.signIndex() : "");
       else
        return process.nonsignatoryname();
    },
    top : function() {
        var top = $("<div class='top'/>");
        top.text(this.name().toUpperCase());
        return top;
    },
    fieldView : function (args) {
        return new FieldBasicDesignView(args);
    },
    postRender : function() {
        // Use this to bind extra post basic rendering in subclasses
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
                        fields.append(view.fieldView(
                                        { model : field,
                                          el : $("<div/>")
                                        }).el);
                            };
        makeField(signatory.field("fstname"));
        makeField(signatory.field("sndname"));
        makeField(signatory.field("email"));
        _.each(signatory.fields(),function(field){
            if (field.name() != "fstname" &&
                field.name() != "sndname" &&
                field.name() != "email"   &&
                !field.isSignature())
                makeField(field);
        });
        this.container.append(fields);
        this.postRender();
        return this;
    }
});

window.SignatoryDesignViewAdvanced = SignatoryDesignViewBasic.extend({
   addFieldButton : function() {
        var signatory = this.model;
        var addFieldButton = $("<a class='addField' href='#'/>");
        addFieldButton.click(function(){
            signatory.addNewField();
            return false;
            });
        return addFieldButton;
   } ,
   fieldView : function (args) {
        return new FieldAdvancedDesignView(args);
   },
   refreshRoleSelector: function() {
       var view = this;
       var signatory = this.model;

       if (view.showRoleSelector) {
           this.setRoleIcon.addClass("selected");
           this.setRoleBox = $("<div class='setRoleBox'/>");
           this.container.append(this.setRoleBox);

           var closeButton = $("<a class='modal-close close' href='#'/>");
           closeButton.click(function() {
               view.setRoleIcon.removeClass("selected");
               if (view.setRoleBox!= undefined)  view.setRoleBox.remove();
               view.showRoleSelector = false;
               return false;
           });
           this.setRoleBox.append(closeButton);


           var makeSignatoryWrapper = $("<div class='option'/>");
           var makeSignatoryCheckbox =  $("<input class='radio' type='radio'/>");
           if (signatory.signs())
               makeSignatoryCheckbox.attr("checked","yes");
           else
               makeSignatoryCheckbox.click(function() {
                   signatory.makeSignatory();
            });
           makeSignatoryWrapper.append(makeSignatoryCheckbox).append("<span class='radiolabel'>"+localization.isSigningParty+"</span>");
           this.setRoleBox.append(makeSignatoryWrapper);


           var makeNotSignatoryWrapper = $("<div class='option'>");
           var makeNotSignatoryCheckbox =  $("<input class='radio' type='radio'>");
           makeNotSignatoryWrapper.append(makeNotSignatoryCheckbox).append("<span class='radiolabel'>"+localization.isNotSigningParty+"</span>");
           this.setRoleBox.append(makeNotSignatoryWrapper);
           if (!signatory.signs())
               makeNotSignatoryCheckbox.attr("checked","yes");
           else
               makeNotSignatoryCheckbox.click(function() {
                   signatory.makeViewer();
            });
       }
       else {
           this.setRoleIcon.removeClass("selected");
           if (this.setRoleBox!= undefined)  this.setRoleBox.remove();
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
          signatory.document().view.toggleSignOrger();
            });
        return setSignOrderIcon;
   },
   signOrderSelector : function() {
      var select = $("<select class='selectSignOrder'/>");
      var signatory = this.model;
      var signatoriesCount = signatory.document().signatories().length;
      for(var i=1;i<=signatoriesCount;i++)
      {
        if (signatory.author() && (i != 1 && i != signatoriesCount ))  continue; // Author can only select first or last.
        var option = $("<option value='"+i+"'>"+i+"</option>");
        if (i == signatory.signorder())
            option.attr("selected","Yes");
        select.append(option);
      }
      select.change(function(){
            signatory.setSignOrder($(this).val());
      });
      return select;
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
   placeSignatureIcon : function() {
       var view = this;
       var signatory = this.model;
       var field = signatory.field("signature");
       var placeSignatureIcon = $("<a class='placeSignatureIcon' href='#'/>");
       field.view = placeSignatureIcon;
       field.view.redborder = function() {placeSignatureIcon.addClass('redborder')};
       field.view.mousedown(function() {placeSignatureIcon.removeClass('redborder')});
       var fileview = signatory.document().mainfile().view;
       placeSignatureIcon.draggable({
                    handle: ".ddIcon",
                    appendTo: "body",
                    helper: function(event) {
                        return new FieldPlacementView({model: field, el : $("<div/>")}).el;
                    },
                    start: function(event, ui) {
                    },
                    stop: function() {
                    },
                    drag: function(event, ui) {
                    },
                    onDrop: function(page, x,y ){
                          field.addPlacement(new FieldPlacement({
                              page: page.number(),
                              fileid: page.file().fileid(),
                              field: field,
                              x : x,
                              y : y
                            }));
                    }
            });

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
            top.append(this.placeSignatureIcon());
        top.append(this.addFieldButton());
        if (signatory.signs() && document.view.signOrderVisible())
            top.append(this.signOrderSelector());


        return top;
    },
    postRender : function() {
        this.refreshRoleSelector();
    }

});

window.SignatoriesDesignBasicView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('reset', this.render);
        this.model.bind('change', this.render);
        this.extra = args.extra;
        this.render();
    },
    render: function(){
        var box = $(this.el);
        box.addClass('signatoriesbox');
        var document = this.model;
        box.children().detach();
        var s1view = new SignatoryDesignViewBasic({model: document.signatories()[0], el: $("<div/>")});
        var s2view = new SignatoryDesignViewBasic({model: document.signatories()[1], el: $("<div/>")});
        box.append(s1view.el);
        box.append(s2view.el);
        box.append(this.extra);
        return this;
    },
    showSignatory : function() {
        return true;
    }
});

window.SignatoriesDesignAdvancedView = SignatoriesDesignBasicView.extend({
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
        var header = $("<a href='#' class='header'/>").text(localization.signatoryListPartner);
        if (document.view.signOrderVisible())
            header.append($("<span class='float-right'/>").text(localization.signatoryListSignOrder));
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
                sigline.append($("<span/>").text(text));
                if (document.view.signOrderVisible())
                  sigline.append($("<span class='float-right'/>").text(number));
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
            var s1view = new SignatoryDesignViewAdvanced({model: document.signatories()[0], el: $("<div/>")});
            box.append(s1view.el);
            box.append($("<div class='sigview dummy'/>"));
        }
        else if (document.signatories().length == 2)
        {
            var s1view = new SignatoryDesignViewAdvanced({model: document.signatories()[0], el: $("<div/>")});
            var s2view = new SignatoryDesignViewAdvanced({model: document.signatories()[1], el: $("<div/>")});
            box.append(s1view.el);
            box.append(s2view.el);

        }
        else
        {
           box.append(this.signatoriesList());
           var sview = new SignatoryDesignViewAdvanced({model: this.current, el: $("<div/>")});
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
