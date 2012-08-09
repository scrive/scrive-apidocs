/*
 * Pagination for lists + persistance
 */


(function(window) {

    window.Paging = Backbone.Model.extend({
        defaults: {
            // first visible item index
            itemMin: 0,
            // last visible item index. If itemMax < itemMin, there are no items
            itemMax: 0,
            pageCurrent: 0,
            pageSize: 0,
            // Maximal number of pages that are sown
            maxNextPages : 5,
            showLimit : undefined
        },
        disabled: function() {
            return this.get("disabled") != undefined && this.get("disabled") == true;
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
        changePageFunction: function(i) {
            var paging = this;
            return function() { paging.changePage(i); };
        },
        updateWithServerResponse: function(resp) {
            this.set(resp);
        },
        showLimit : function() {
            if (this.get("showLimit") != undefined && this.get("pageSize") != undefined)
                return Math.min(this.get("showLimit"),this.get("pageSize"))
            else if (this.get("showLimit") != undefined)
                return this.get("showLimit");
            else
                return this.get("pageSize")
        },
        setShowLimit : function(i) {
            this.set({ "showLimit": i });
        }
    });

    window.PagingView = Backbone.View.extend({
        model: Paging,
        initialize: function(args) {
            _.bindAll(this, 'render');
            var view = this;
            this.model.bind('change', function(){view.render();});
            this.render();
        },
        render: function() {
            $(this.el).empty();
            var paging = this.model;
            var main = $("<div class='pages'>");
            var pages = $("<div />");
            var writePage = function(t,n) {
                var a = $("<span class='page-change' />").text(t);
                a.click(paging.changePageFunction(n));
                pages.append(a);
                return a;
            }
            var maxNextPages = paging.maxNextPages();
            var maxPage = paging.pageCurrent() + maxNextPages - 1;
            for(var i=0;i < maxPage && i*paging.pageSize() < paging.itemMax();i++) {
                var a = writePage((i+1)+"", i);
                if (i == paging.pageCurrent())
                    a.addClass("current");
            }
            if (maxPage*paging.pageSize() < paging.itemMax())
                writePage(" > ", maxPage);
    
            main.append(pages);

            $(this.el).append(main);
        }
    });


})(window);
