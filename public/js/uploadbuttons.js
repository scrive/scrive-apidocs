/* Upload buttons
 * Usage
 *  var button =  UploadButton.init({
 *                   width: 100 // Expected size of the button. 
 *                   name: "Name that will be used for input element"
 *                   text: "Text that will be put inside of button",
 *                   submitOnUpload : Bool //Submit the parent form when file selected })
 *                   list : jQuery("selector") // If you want to show list of uploaded files
 *                   maxlength : 1 // Number of files that can be selected   
 *  will return UploadButton object.
 *
 * It exports method input that returns jQuery object to be inserted anywere you want
 *
 *  button.input()
 *
 *  ! FOR now this module is based on MultiFile jQuery plugin, but this may change soon.
 *  We do not store internally any data about uploaded files.
 * 
*/

(function( window){

var UploadButtonModel = Backbone.Model.extend({
  defaults : {
      name : "",
      text : "",
      width: 200,
      maxlength : 1,
      submitOnUpload : false
  },
  width : function(){
       return this.get("width");
  },
  text: function() {
       return this.get("text");
  },
  name: function() {
       return this.get("name");
  },
  maxlength : function() {
        return this.get("maxlength");
  },
  submitOnUpload : function(){
       return this.get("submitOnUpload") == true;
  },
  list : function() {
        return this.get("list");
  }
});

/* View redners a button and transparent file-input is floats over the button.
 * We initiate Multifile extension on this file input, customising some actions
 */


var UploadButtonView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        this.render();
    },
    render: function () {
        var button = $("<a/>");
        var model = this.model;
        button .addClass("green").addClass("btn-small").css("overflow", "hidden").css("width",model.width() + "px");
        var left  = $("<div class='left'/>");
        var label = $("<div class='label'/>").text(model.text()).css("width",(model.width() - 2* 16) + "px");
        var right = $("<div class='right'/>");
        button .append(left);
        button .append(label);
        button .append(right);
        var fileinput = $("<input class='multiFileInput' type='file'/>");
        fileinput.attr("accept","application/pdf").attr("maxlength",model.maxlength()).attr("name",model.name())
        fileinput.css("width",model.width()  + "px");
        var list = model.list();
        if (list == undefined)
            {
               list = $("<div style='display:none'>");
               button.append(list) ;
            }
        fileinput.MultiFile({
            list: list,
            onFileAppend: function() {
                if (model.submitOnUpload()) {
                    displayLoadingOverlay(localization.loadingFile);
                     button .parents("form").submit();
                }
            }
        });
        button .append(fileinput);
        this.el.append(button);
        return this;
    }
});

/* We add extra div here for some browsers compatibility */
window.UploadButton = {
    init: function (args) {
          var model = new UploadButtonModel(args);
          var input = $("<div style='position:relative;overflow:hidden'/>");
          var view = new UploadButtonView({model : model, el : input});
          return new Object({
              input : function() {return input;}
            });
        }
}
})(window); 
