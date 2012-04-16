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
        url : "",
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
            k == "onSend";
    },
    add: function(k,v)
    {
        var update = {};
        update[k] = v;
        this.set(update);
        return this;
    },
    addInputs : function(v)
    {
        this.set({inputs : this.get("inputs").add(v)});
    },
    remove : function(k) {
        this.set({inputs : this.get("inputs").remove(k)});
        return this;
    },
    sendAjax : function(callback) {
        this.set({ajax : true});
        if (callback != undefined)
            this.set({ajaxsuccess : callback});
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
        if (readCookie("xtoken") != undefined)
            form.append( $('<input type="hidden" name="xtoken">').val(readCookie("xtoken")));
        form.append(this.get("inputs"));
        $("body").append(form);

        if (this.get('ajax'))
            form.ajaxForm({success: this.get('ajaxsuccess'),
                           error: this.get('ajaxerror'),
                           dataType: this.get('expectedType')});

        this.get('beforeSend')();
        form.submit();
        this.get('onSend')();
    },
  success: function(f) {
    this.set({ajaxsuccess: f || function(){}}, {silent:true});
    return this;
  }
});
})(window);
