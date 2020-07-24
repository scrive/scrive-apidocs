var Backbone = require("backbone");
var jQuery = require("jquery");
var _ = require("underscore");
var $ = require("jquery");
var BrowserInfo = require("./utils/browserinfo.js").BrowserInfo;
var Track = require("../scripts/common/track");

/* Use Submit object to skip usage of form elements.
 * Usage
 *  var new Submit({
 *              url: "/docs/123/789"
 *              method: "POST"
 *              });
*/


var Submit = exports.Submit = Backbone.Model.extend({
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

        expectedType: "json"
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
    hasFile : function() {
       return this.get("inputs").filter(":file").size() > 0 || this.get("inputs").find(".file").size() > 0;
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
        var self = this;
        var form = $("<form style='display: none;'/>");
        form.attr("action", this.get("url"));
        form.attr("method", this.get("method"));
        if (this.get("method") == "POST" && this.hasFile())
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
        if (Cookies.get("xtoken") != undefined && this.get("method").toUpperCase() == "POST") {
          var xtoken = Cookies.getMulti("xtoken").join(";");
          form.append( $('<input type="hidden" name="xtoken">').val(xtoken));
        }
        form.append(this.get("inputs"));
        $("body").append(form);

        if (this.get('ajax'))
            form.ajaxForm({success: function(p1,p2,p3,p4) {
                              if (self.hasFile() && BrowserInfo.isIE9orLower() && typeof p1 === 'string') {
                                // file uploads are handled through iframe in <=IE9
                                // which means, that even 400 error on the backend is handled with success callback
                                // let's try checking if we got a crappy html 400 response.
                                // Additionally, this callback (if really successful) is called
                                // with JSON response wrapped in <pre></pre>
                                p1 = p1.trim();
                                if (p1.substring(0, 5).toLowerCase() === '<pre>') {
                                  p1 = p1.substring(5, p1.length);
                                }
                                if (p1.substring(p1.length - 6, p1.length).toLowerCase() === '</pre>') {
                                  p1 = p1.substring(0, p1.length - 6);
                                }
                              }

                              if (!p1) {
                                // Default to an empty object if the response
                                // content is empty.
                                p1 = {};
                              }

                              if (typeof p1 === "string") {
                                try {
                                  p1 = JSON.parse(p1);
                                } catch(e) {
                                  self.get('ajaxerror')(p1,p2,p3,p4);
                                  form.remove();
                                  return;
                                }
                              }

                              try {
                                self.get('ajaxsuccess')(p1,p2,p3,p4);
                              } catch (e) {
                                if (window.trackJs !== undefined && trackJs) {
                                  trackJs.track(e);
                                }
                                throw e;
                              }
                              form.remove();
                           },
                           error: function(p1,p2,p3,p4) {
                             try {
                               p1["url"] = self.get("url");
                               self.get('ajaxerror')(p1,p2,p3,p4);
                             } catch (e) {
                               if (window.trackJs !== undefined && trackJs) {
                                 trackJs.track(e);
                               }
                               throw e;
                             }
                             form.remove();
                           },
                           dataType: this.get('expectedType'),
                           timeout : this.get('ajaxtimeout')
            });

        this.get('beforeSend')();
        if(this.mixpanel())
          Track.track_timeout(this.mixpanel().name, this.mixpanel().props, function() {
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

