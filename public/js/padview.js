/* Signatory view of document
 * Now unified with author and viewer views
 */


(function(window){

    
    
window.PadQueue = Backbone.Model.extend({
    defaults: {
        ready : false
    },
    initialize: function (args) {
        var padqueue = this;
        this.url = "padqueue/state";
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
    magichash : function() {
        return this.get("magichash");
    },
    hasDocument : function() {
        return this.documentid() != undefined && this.signatorylinkid() != undefined && this.magichash() != undefined;
    },
    parse: function(args) {
     return {
      documentid : args.documentid,
      signatorylinkid: args.signatorylinkid,
      magichash : args.magichash,
      ready: true
      };
    }

});
    
    
window.PadQueueView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('reset', this.render);
        this.model.bind('change', this.render);
        this.model.view = this;
        this.render();
    },
    documentView : function() {
        var padqueue = this.model;
        var doc =  KontraStandardDocument.init({
                            id: padqueue.documentid(),
                            viewer : new DocumentViewer({
                                signatoryid : padqueue.signatorylinkid(),
                                magichash : padqueue.magichash()
                            })
                        });
        return doc.view.el;
    },
    noDocumentView : function() {
        var box = $("<div class='noDocumentAvaible'> </div>");
        var header = $("<div class='header'>No document is avaible</div>")
        var sheader = $("<div class='sheader'>Waiting for device owner to send a document</div>");
        box.append(header).append(sheader);
        return box;
        
    },
    
    render: function () {
        var padqueue = this.model;
        var container = this.el;
        container.empty();
        if (padqueue.ready()) {
            if (padqueue.hasDocument())
                container.append(this.documentView())
            else 
                container.append(this.noDocumentView())      
        }
        return this;

    }
});

window.KontraPad = {
    init : function(args){
       this.model = new PadQueue({ });
       this.view = new PadQueueView({
                        model: this.model,
                        el : $("<div/>")
                    });
       return this;
   },
   recall : function()
   {
       this.model.recall();
   }
};
})(window);
