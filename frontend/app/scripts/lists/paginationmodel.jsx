/** @jsx React.DOM */

define(['legacy_code'], function() {


return  Backbone.Model.extend({
        defaults: {
            // first visible item index
            itemMin: 0,
            // last visible item index. If itemMax < itemMin, there are no items
            itemMax: 0,
            pageCurrent: 0,
            pageSize: 0,
            // Maximal number of pages that are sown
            maxNextPages : 5
        },
        itemMin: function() {
            return this.get("itemMin");
        },
        itemMax: function() {
            return this.get("itemMax");
        },
        maxNextPages:function() {
            return this.get("maxNextPages");
        },
        pageCurrent: function() {
            return this.get("pageCurrent");
        },
        pageSize: function() {
            return this.get("pageSize");
        },
        changePage: function(i) {
            this.set({ "pageCurrent": i });
        },
        switchToEarlierPageIfThatsNeeded : function() {
           if(this.pageCurrent() > 0 && (this.itemMax() <  (this.pageCurrent()) * this.pageSize()))
             this.changePage(this.pageCurrent() - 1);
        }
    });


});

