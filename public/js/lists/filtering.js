/*
 * Filtering options- for now its just text search and persistance
 */


(function(window) {

    window.TextFiltering = Backbone.Model.extend({
        type : function() {
            return "text";
        },
        disabled: function() {
            return this.get("disabled") != undefined && this.get("disabled") == true;
        },
        text: function() {
            return this.get("text");
        },
        searchText: function(text) {
            SessionStorage.set(this.get("namespace"), "textfiltering", text);
            this.set({ text: text });
        },
        infotext: function() {
            return this.get("infotext");
        },
        setSessionStorageNamespace: function(namespace) {
            this.set({ namespace: namespace });
            if (SessionStorage.get(namespace, "textfiltering") != undefined && ! this.disabled()) {
                this.searchText(SessionStorage.get(namespace, "textfiltering"));
            }
        }
    });

  window.SelectFiltering = Backbone.Model.extend({
        defaults : {
            options: [{name : "", value : ""}],
            name: ""
        },
        type : function() {
            return "select"
        },
        initialize: function(args) {
        },
        options: function() {
            return this.get("options");
        },
        name: function() {
            return this.get("name");
        },
        textWidth: function() {
            return this.get("textWidth");
        },
        selected: function() {
            return this.get("selected");
        },
        find : function(v){
            return _.filter(this.options(), function(o) {return o.value == v})[0];
        },
        select: function(v) {
            if (this.find(v) != undefined) {
               SessionStorage.set(this.get("namespace"), "select-filtering" + this.name(), this.find(v).value );
               this.set({"selected" : this.find(v)});
            }
            else
                this.select("");
        },
        selectedValue : function() {
          if (this.selected() != undefined)
              return this.selected().value;
          else
              return "";
        },
        setSessionStorageNamespace: function(namespace) {
            this.set({ namespace: namespace });
            var fss = SessionStorage.get(namespace, "select-filtering" + this.name());
            if (fss == undefined) fss = "";
                this.select(fss);
        }
    });

   window.SelectAjaxFiltering = Backbone.Model.extend({
        defaults : {
            name: "",
            defaultName : ""
        },
        type : function() {
            return "ajax-select";
        },
        initialize: function(args) {
            this.set({"options" : [{name : this.defaultName(), value : ""}]});
        },
        defaultName : function() {
            return this.get("defaultName");
        },
        options: function() {
            return this.get("options");
        },
        name: function() {
            return this.get("name");
        },
        textWidth: function() {
            return this.get("textWidth");
        },
        selected: function() {
            return this.get("selected");
        },
        optionsURL : function() {
            return this.get("optionsURL");
        },
        optionsParse : function() {
            return this.get("optionsParse");
        },
        find : function(v){
            return _.filter(this.options(), function(o) {return o.value == v})[0];
        },
        needMoreOptions: function() {
            return this.options().length < 2;
        },
        getMoreOptions: function(callback) {
            var self = this;
            $.get(self.optionsURL(), function(res) {
              var currentValue = self.selectedValue();
              var newOptions = self.optionsParse()(JSON.parse(res));
              newOptions.unshift({name : self.defaultName(), value : ""})
              self.set({options : newOptions}, {silent : true});
              self.select(currentValue);
              self.trigger("more-options-available"); // We might not rendered with select, since probably we just changed options, but not selected one.
              callback();
            })
        },
        select: function(v) {
            if (this.find(v) != undefined) {
               SessionStorage.set(this.get("namespace"), "ajax-select-filtering-name" + this.name(), this.find(v).name );
               SessionStorage.set(this.get("namespace"), "ajax-select-filtering-value" + this.name(), this.find(v).value );
               this.set({"selected" : this.find(v)}, {silent : this.selectedValue() == v});
            }
            else
                this.select("");
        },
        selectedValue : function() {
          if (this.selected() != undefined)
              return this.selected().value;
          else
              return "";
        },
        setSessionStorageNamespace: function(namespace) {
            this.set({ namespace: namespace });
            var fssn = SessionStorage.get(namespace, "ajax-select-filtering-name" + this.name());
            var fssv = SessionStorage.get(namespace, "ajax-select-filtering-value" + this.name());
            if (fssn == undefined || fssv == "") fssn = this.defaultName();
            if (fssv == undefined) fssv = "";
            this.set({"options" : [{name : fssn, value : fssv}]});
            this.select(fssv);
        }
    });



  window.IntervalDoubleSelectFiltering = Backbone.Model.extend({
        defaults : {
            options: [{name : "", value : "<"}, {name : "", value : ">"}],
            name: "",
            selectedBottomPrefix : "",
            selectedTopPrefix : ""
        },
        type : function() {
            return "interval-double-select";
        },
        options: function() {
            return this.get("options");
        },
        name: function() {
            return this.get("name");
        },
        textWidth: function() {
            return this.get("textWidth");
        },
        selectedBottomPrefix : function() {
            return this.get("selectedBottomPrefix");
        },
        selectedTopPrefix : function() {
            return this.get("selectedTopPrefix");
        },
        selectedTop: function() {
            return this.get("selectedTop");
        },
        selectedBottom: function() {
            return this.get("selectedBottom");
        },
        find : function(v){
            return _.filter(this.options(), function(o) {return o.value == v})[0];
        },
        selectTop: function(v) {
            console.log("Selecting top");
            if (this.find(v) != undefined) {
               SessionStorage.set(this.get("namespace"), "interval-double-select-top" + this.name(), this.find(v).value );
               this.set({"selectedTop" : this.find(v)});
            }
            else
                this.selectTop(">");
        },
        selectBottom: function(v) {
            console.log("Selecting top");
            if (this.find(v) != undefined) {
               SessionStorage.set(this.get("namespace"), "interval-double-select-bottom" + this.name(), this.find(v).value );
               this.set({"selectedBottom" : this.find(v)});
            }
            else
                this.selectBottom("<");
        },
        selectedValue : function() {
              var valueBottom = this.selectedBottom().value != "<" ? "Just " + this.selectedBottom().value : "Nothing";
              var valueTop = this.selectedTop().value != ">"       ? "Just " + this.selectedTop().value : "Nothing";
              return "(" + valueBottom + "," + valueTop + ")";
        },
        setSessionStorageNamespace: function(namespace) {
            this.set({ namespace: namespace });
            var fss = SessionStorage.get(namespace, "interval-double-select-top" + this.name());
            if (fss == undefined) fss = ">";
                this.selectTop(fss);
            fss = SessionStorage.get(namespace, "interval-double-select-bottom" + this.name());
            if (fss == undefined) fss = "<";
                this.selectBottom(fss);
        }
    });


    window.FilteringView = Backbone.View.extend({
    initialize: function (args) {
        if (args.model.type() == "text")
            return new TextFilteringView(args);
        else if (args.model.type() == "select")
            return new SelectFilteringView(args);
        else if (args.model.type() == "ajax-select")
            return new SelectAjaxFilteringView(args);
        else if (args.model.type() == "interval-double-select")
            return new IntervalDoubleSelectFilteringView(args);
    }
    });

    var TextFilteringView = Backbone.View.extend({
        model: TextFiltering,
        initialize: function(args) {
            _.bindAll(this, 'render');
            var view = this;
            this.render();
        },
        render: function() {
            var filtering = this.model;
            var searchBox = InfoTextInput.init({ infotext: filtering.infotext(),
                                                 value: filtering.text(),
                                                 cssClass: "list-search float-right" });
            searchBox.input().keypress(function(event) {
                var keycode = (event.keyCode ? event.keyCode : event.which);
                if (keycode == '13') {
                    mixpanel.track('Search',
                                   {Input : "Enter",
                                    Query : $(this).val()});
                    filtering.searchText($(this).val());
                }});
            var button = Button.init({color: "black",
                                      size: "tiny",
                                      text: localization.searchBoxButtonText,
                                      cssClass: "float-right search-button",
                                      onClick: function() {
                                          mixpanel.track('Search',
                                                         {Query : searchBox.value(),
                                                          Input : "Button"});
                                          filtering.searchText(searchBox.value());
                                      }
                                     });
            $(this.el).append(button.input()).append(searchBox.input());
        }
    });

    var SelectFilteringView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            var view = this;
            this.model.bind('change', function() {view.render();})
            this.render();
        },
        render: function() {
            $(this.el).empty();
            var selectFiltering = this.model;
            var options = this.model.options();
            var selected = selectFiltering.selected();
            var selectOptions = [];
            for(var i=0;i< options.length;i++)
                    if (options[i] != selected)
                         selectOptions.push(options[i])
            if (this.select != undefined)
                this.select.clear();
            this.select = new Select({
                options : selectOptions,
                name : selected.name,
                theme : selected.value != "" ? "dark" : "warm",
                textWidth : selectFiltering.textWidth(),
                onSelect : function(value) {
                    mixpanel.track('Filter ' + selectFiltering.name(),
                                   {Value : selected.name});
                    selectFiltering.select(value);
                }
            });
            $(this.el).append(this.select.view().el);
        }
    });


    var SelectAjaxFilteringView = Backbone.View.extend({
        model: SelectAjaxFiltering,
        initialize: function(args) {
            _.bindAll(this, 'render');
            var view = this;
            this.model.bind('change', function() {view.render();})
            this.model.bind('more-options-available', function() {view.render();})
            this.render();
        },
        render: function() {
            var self = this;
            $(this.el).empty();
            var selectFiltering = this.model;
            var options = this.model.options();
            var selected = selectFiltering.selected()
            var selectOptions = [];
            for(var i=0;i< options.length;i++)
                    if (options[i] != selected)
                         selectOptions.push(options[i])
            if (this.select != undefined)
                this.select.clear();
            this.select = new Select({
                options : selectOptions,
                name : selected.name,
                theme : selected.value != "" ? "dark" : "warm",
                textWidth : selectFiltering.textWidth(),
                onOpen : function() {
                    if (!selectFiltering.needMoreOptions())  return true;
                    selectFiltering.getMoreOptions(function() {self.select.open();})
                    return false;
                },
                onSelect : function(value) {
                    mixpanel.track('Filter ' + selectFiltering.name(),
                                   {Value : selected.name});
                    selectFiltering.select(value);
                }
            });
            $(this.el).append(this.select.view().el);
        }
    });


    var IntervalDoubleSelectFilteringView = Backbone.View.extend({
        model: SelectFiltering,
        initialize: function(args) {
            _.bindAll(this, 'render');
            var view = this;
            this.model.bind('change', function() {view.render();})
            this.render();
        },
        render: function() {
            $(this.el).empty();
            var filtering = this.model;
            var options = this.model.options();
            var selectedBottom = filtering.selectedBottom()
            var selectedTop = filtering.selectedTop()

            var selectOptionsBottom = [];
            for(var i=0;i< options.length;i++) {
                    if (options[i] != selectedBottom && options[i].value != ">")
                        selectOptionsBottom.push({name : options[i].name, value : options[i].value});
                    if (options[i] == selectedTop) break;
            }
            if (selectOptionsBottom[0].value == "<")
                selectOptionsBottom.push(selectOptionsBottom.shift());
            selectOptionsBottom.reverse();

            if (this.selectBottom != undefined)
                this.selectBottom.clear();
            this.selectBottom = new Select({
                options : selectOptionsBottom,
                name :  selectedBottom.value != "<" ? filtering.selectedBottomPrefix() + " "  + selectedBottom.name : selectedBottom.name,
                theme : selectedBottom.value != "<" ? "dark" : "warm",
                textWidth : filtering.textWidth(),
                onSelect : function(value) {
                    mixpanel.track('Select from date',
                                   {Value : selectedBottom.value});
                    filtering.selectBottom(value);
                }
            });
            $(this.el).append($(this.selectBottom.view().el).addClass("float-left"));

            var selectOptionsTop = [];
            var addOption = false;
            for(var i=0;i< options.length;i++) {
                    if (options[i] == selectedBottom) addOption = true;
                    if (options[i].value != "<" && addOption && options[i] != selectedTop)
                         selectOptionsTop.push({name : options[i].name, value : options[i].value});
            }
            selectOptionsTop.reverse();
            if (this.selectTop != undefined)
                this.selectTop.clear();
            this.selectTop = new Select({
                options : selectOptionsTop,
                name : selectedTop.value != ">" ? filtering.selectedTopPrefix() + " " + selectedTop.name : selectedTop.name,
                theme : selectedTop.value != ">" ? "dark" : "warm",
                textWidth : filtering.textWidth(),
                onSelect : function(value) {
                    mixpanel.track('Select to date',
                                   {Value : selectedTop.value});
                    filtering.selectTop(value);
                }
            });
            $(this.el).append($(this.selectTop.view().el).addClass("float-left"));

        }
    });
})(window);
