/*
 * Sorting module for lists
 * It is ussed to define on what we want to sort and in what order.
 * Provides session persistance
 */


(function(window) {
    window.Sorting = Backbone.Model.extend({
        defaults: {
            fields: [],
            current: undefined,
            order: true
        },
        disabled: function() {
            return this.get("disabled") != undefined && this.get("disabled") == true;
        },
        fields: function() {
            return this.get("fields");
        },
        isCurrent: function(field) {
            return this.get("current") != undefined && this.get("current") == field;
        },
        current: function() {
            return this.get("current");
        },
        isAsc: function() {
            return this.get("order") != undefined && this.get("order") == true;
        },
        isDesc: function() {
            return this.get("order") != undefined && this.get("order") == false;
        },
        isSortable: function(field) {
            return _.any(this.fields(), function(f) { return f == field; });
        },
        sortOn: function(field) {
            if (this.isCurrent(field)) {
                this.set({ order: !this.get("order") });
                this.updateSessionStorage();
            } else {
                this.set({order: true, current: field});
                this.updateSessionStorage();
            }
        },
        sortOnFunction: function(field) {
            var sorting = this;
            return function() { sorting.sortOn(field); };
        },
        updateSessionStorage: function() {
            SessionStorage.set(this.get("namespace"), "sorting", this.current());
            SessionStorage.set(this.get("namespace"), "sorting_order", this.isAsc() + "");
        },
        setSessionStorageNamespace: function(namespace) {
            this.set({namespace: namespace});
            if (SessionStorage.get(namespace, "sorting") != undefined &&
                   SessionStorage.get(namespace, "sorting_order") != undefined &&
                   !this.disabled()) {
                    this.set({
                        current: SessionStorage.get(namespace, "sorting"),
                        order: SessionStorage.get(namespace, "sorting_order") == "true"
                    });
            }
        }
    });
})(window);
