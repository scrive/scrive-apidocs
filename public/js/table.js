(function(window){

    
    
window.TableModel = Backbone.Model.extend({
    defaults: {
        fields : []
    },
    initialize: function (args) {
    },
    fields : function() {
        return this.get("fields")
    },
    field : function(i,j) {
       if (this.fields()[i] != undefined) 
          return this.fields()[i][j]
       return undefined;   
    },
    rows: function(){
        return this.fields().length;
    },
    cols: function() {
        var max = 0;
        _.each(this.fields(), function(field) {
          if (field!= undefined && field.length > max)
            max = field.length;
        })
        return max;
    }

});
    
    
window.TableView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    render: function () {
        var model = this.model;
        this.el.children().detach();
        var table = $("<table/>");
        var tbody = $("<tbody/>")
        for(var i =0; i < model.rows(); i++)
        {
            var tr = $("<tr/>")
            for(var j =0; j < model.cols(); j++)
            {
                var td = $("<td>");
                var v = model.field(i,j);
                if (v == undefined)
                    ;
                else if ("string" == typeof v)
                    td.text(v);
                else 
                    td.append(v);
                tr.append(td)
            }
            tbody.append(tr);
        }    
        table.append(tbody);
        this.el.append(table);
        return this;

    }
});

window.Table = {
    init : function(args){
       this.model = new TableModel({
                        fields : args.fields
                    });
       this.view  = new TableView({
                        model: this.model,
                        el : $("<div/>")
                    });
       return this;
   }
};
})(window);
