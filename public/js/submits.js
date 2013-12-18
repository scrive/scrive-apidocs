/* Use Submit object to skip usage of form elements.
 * Usage
 *  var new Submit({
 *              url: "/docs/123/789" | on what to set the tooltip
 *              method: "" | text to be put there
 *              });
*/

(function( window){
window.Submit = Backbone.Model.extend({
    defaults : {
        url : window.location, // Fix for IE - new version can cut off last part of url, if not followed by slash
        method : "GET",
        inputs : jQuery(),
        onSend : function () {},
        beforeSend : function () {},
        // for ajax submission
        ajax : false,
        ajaxsuccess: function() {},
        ajaxerror: function() {},

        expectedType: null
    },
    ignored : function(k)
    {
      return k == "url" ||
            k == "method" ||
            k == "inputs" ||
            k == "ajax" ||
            k == "ajaxsuccess" ||
            k == "ajaxerror" ||
            k == "expectedType" ||
            k == "beforeSend" ||
            k == "onSend"    ||
            k == "mixpanel" ||
            k == "ajaxtimeout";
    },
    add: function(k,v)
    {
        var update = {};
        update[k] = v;
        this.set(update);
        return this;
    },
    addMany : function(o) {
        var self = this;
        _.each(o, function(v,k) {self.add(k,v);});
        return this;
    },
    addInputs : function(v)
    {
        this.set({inputs : this.get("inputs").add(v)});
        return this;
    },
    remove : function(k) {
        this.set({inputs : this.get("inputs").remove(k)});
        return this;
    },
    sendAjax : function(callback,errorcallback) {
        this.set({ajax : true});
        if (callback != undefined)
            this.set({ajaxsuccess : callback});
        if (errorcallback != undefined)
            this.set({ajaxerror : errorcallback});
        return this.send();
    },
    send: function() {
        var form = $("<form style='display:hidden'/>");
        form.attr("action", this.get("url"));
        form.attr("method", this.get("method"));
        if (this.get("method") == "POST")
            form.attr("enctype","multipart/form-data");
        for (var k in this.attributes) {
            var val = this.get(k);
            if (this.ignored(k) || val == undefined)
                continue ;
            else if (!$.isArray(val)) {
                var input = $("<input type='hidden' />");
                input.attr("name", k);
                input.attr("value", val);
                form.append(input);
            } else for (var i=0;i<val.length;i++) {
                var input = $("<input type='hidden' />");
                input.attr("name", k);
                input.attr("value", val[i]);
                form.append(input);
            }
        }
        if (Cookies.get("xtoken") != undefined && this.get("method").toUpperCase() == "POST")
            form.append( $('<input type="hidden" name="xtoken">').val(Cookies.get("xtoken")));
        form.append(this.get("inputs"));
        $("body").append(form);

        if (this.get('ajax'))
            form.ajaxForm({success: this.get('ajaxsuccess'),
                           error: this.get('ajaxerror'),
                           dataType: this.get('expectedType'),
                           timeout : this.get('ajaxtimeout')
            });

        this.get('beforeSend')();
        if(this.mixpanel())
          trackTimeout(this.mixpanel().name, this.mixpanel().props, function() {
              form.submit();
          });
        else
            form.submit();
        this.get('onSend')();
    },
  success: function(f) {
    f = f || function(){};
    this.set({ajaxsuccess: f}, {silent:true});
    return this;
  },
  mixpanel: function() {
    return this.get('mixpanel');
  }
});
})(window);
