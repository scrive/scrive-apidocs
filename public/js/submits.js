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
        inputs : jQuery()
    },
    ignored : function(k)
    {
      return k == "url" || k == "method" || k == "inputs";
    },
    add: function(k,v)
    {
        this.set({k : v});
    },
    addInputs : function(v)
    {
        this.set({inputs : this.get("inputs").add(v)});
    },
    send: function() {
        var form = $("<form style='display:hidden'/>");
        form.attr("action", this.get("url"));
        form.attr("method", this.get("method"));
        if (this.get("method") == "POST")
           form.attr("enctype","multipart/form-data");
        for (var k in this.attributes) {
        var val = this.get(k)
        if (this.ignored(k))
          continue ;  
        else
          if (!$.isArray(val))
            {var input = $("<input type='hidden' />");
             input.attr("name", k);
             input.attr("value", val);
             form.append(input);
            }
        else 
          for (var i=0;i<val.length;i++)
            {var input = $("<input type='hidden' />");
             input.attr("name", k);
             input.attr("value", val[i]);
             form.append(input);
            }
        
        }
        form.append(this.get("inputs"));
        $("body").append(form);
        form.submit();
    }})
})(window); 
