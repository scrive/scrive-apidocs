/* Signatory view of document
 * Now unified with author and viewer views
 */


(function(window){

    
    
window.PadQueue = Backbone.Model.extend({
    defaults: {
        needFullRefresh  : false,
        ready : false
    },
    initialize: function (args) {
        var padqueue = this;
        this.url = "/padqueue/state";
        padqueue.refresher();
    },
    refresher : function() {
        var padqueue = this;
        this.refresherstarted = true;
        padqueue.recall();
        setTimeout(function() {padqueue.refresher();} , 3000 );
    },
    ready: function(){
        return this.get("ready");
    },
    recall : function() {
       this.fetch({ processData:  true, cache : false});
    },
    documentid : function() {
        return this.get("documentid");
    },
    signatorylinkid : function() {
        return this.get("signatorylinkid");
    },
    hasDocument : function() {
        return this.documentid() != undefined && this.signatorylinkid() != undefined;
    },
    logged : function() {
        return this.loggedToSystem() || this.loggedToPad();
    },
    loggedToSystem : function() {
        return this.get("logged") == "system";
    },
    loggedToPad : function() {
        return this.get("logged") == "pad";
    },
    needFullRefresh : function() {
        return this.get("needFullRefresh");  
    },
    logout : function() {
        return new Submit({method: "POST", url : "/padqueue/logout"});
    },
    parse: function(args) {
     return {
      documentid : args.documentid,
      signatorylinkid: args.signatorylinkid,
      logged : args.logged,
      needFullRefresh : this.documentid() != undefined &&
                        ((this.signatorylinkid() != args.signatorylinkid) || (this.documentid() != args.documentid)),
      ready: true
      };
    }

});
    
    
window.PadQueueView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.view = this;
        this.render();
    },
    documentView : function() {
        var padqueue = this.model;
        var doc =  new DocumentSignView({
                    id: padqueue.documentid(),
                    viewer : new DocumentViewer({
                        signatoryid : padqueue.signatorylinkid()
                      })
                   });
        $('body').prepend(new DocumentSignViewHeader({model : doc.model, mainview : doc.view}).el);
        $('body').append(new DocumentSignViewFooter({model : doc.model,  mainview : doc.view}).el);
        return doc.view.el;
    },
    noDocumentView : function() {
        var box = $("<div class='noDocumentAvaible'> </div>");
        var header = $("<div class='header'>Inget dokument tillgängligt</div>");
        var sheader = $("<div class='sheader'>Inväntar dokument</div>");
        box.append(header).append(sheader);
        return box;
        
    },
    logToPadDevicePopup : function() {
        Login({
            pad : true,
            referer : "/padqueue"
            });
    },
    padLogoutIcon : function() {
        var icon = $("<div class='padTopIcon logout-image' style='float: right;position: absolute;top: 0;right:0'>");
        var padqueue = this.model;
        icon.click(function() {padqueue.logout().send();});
        return icon;
    },
    backToSystemIcon : function() {
        var padqueue = this.model;
        var icon = $("<div class='padTopIcon go-back-image' style='float:right;position: absolute;top: 0;right:0'>");
        icon.click(function() { if (padqueue.hasDocument())
                                 window.location = '/d/' + padqueue.documentid();
                                else
                                 window.location = '/d';});
        return icon;
    },
    render: function () {
        var padqueue = this.model;
        var container = $(this.el);
        container.empty();
        if (padqueue.ready()) {
            if (padqueue.needFullRefresh())
               window.location = window.location; // We reload if content has changes so much that it is not good to keep it opened. 
            else if (padqueue.hasDocument())
                container.append(this.documentView());
            else if (padqueue.logged())
                container.append(this.noDocumentView())  ;    
            else 
                this.logToPadDevicePopup();

            if (padqueue.loggedToPad())
               $('body').append(this.padLogoutIcon());
            if (padqueue.loggedToSystem())
               $('body').append(this.backToSystemIcon());

        }
        return this;

    }
});

window.KontraPad = function() {
     var model = new PadQueue({ });
     var view = new PadQueueView({
                        model: model,
                        el : $("<div/>")
                    });
     return {
         model : function()  {return model;},
         view :  function()  {return view;},
         recall : function() {model.recall();}
    };
};
})(window);
