/* Upload buttons
 * Usage
 *  var button =  new UploadButton({
 *                   width: 100 // Expected size of the button.
 *                   name: "Name that will be used for input element"
 *                   text: "Text that will be put inside of button",
 *                   button: Alternate button element to use,
 *                   submitOnUpload : Bool //Submit the parent form when file selected })
 *                   list : jQuery("selector") // If you want to show list of uploaded files
 *                   maxlength : 1 // Number of files that can be selected
 *  will return UploadButton object.
 *
 * It exports method input that returns jQuery object to be inserted anywere you want
 *
 *  button.el()
 *
 *  ! FOR now this module is based on MultiFile jQuery plugin, but this may change soon.
 *  We do not store internally any data about uploaded files.
 *
 * If you are using onAppend construct you can't submit input in first 10 ms.
*/

define(['Backbone', 'legacy_code'], function() {

var UploadButtonModel = Backbone.Model.extend({
  defaults : {
      name : "",
      text : "",
      style: '',
      button : null,
      width: 200,
      height: 66,
      maxlength : 1,
      submitOnUpload : false,
      size : "small",
      showLoadingDialog : true,
      color: 'green',
      onAppend: undefined, // If set no files will be stored in this upload box
      customcolor: undefined,
      textcolor: undefined
  },
  width : function(){
       return this.get("width");
  },
  height : function(){
       return this.get("height");
  },
  text: function() {
       return this.get("text");
  },
  style: function() {
       return this.get("style");
  },
  button: function() {
       return this.get("button");
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
  },
  size : function() {
        return this.get("size");
  },
  submit : function(){
        return this.get("submit");
  },
  hasSubmit : function(){
        return this.submit() != undefined;
  },
  onAppend : function() {
             return this.get("onAppend");
  },
  type : function() {
        return this.get("type");
  },
  fileType : function() {
        return this.get("fileType");
  },
  color : function() {
        return this.get("color");
  },
  customcolor : function() {
        return this.get("customcolor");
  },
  textcolor : function() {
        return this.get("textcolor");
  },
  shape : function() {
        return this.get("shape");
  },
  cssClass : function() {
        return this.get("cssClass");
  },
  click: function() {
      if(this.get('onClick')) {
          return this.get('onClick')();
      }
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

        var model = this.model;
        var button = model.button();
        if (!button) {
            button = new Button({size: model.size(), type: model.type(), shape : model.shape(), width: model.width(), text: model.text(), style: model.style(), customcolor: model.customcolor(), textcolor: model.textcolor(), onClick : function() {return false;}}).el();
        }
        var fileinput = $("<input class='multiFileInput' type='file'/>");
        if (model.fileType() != "")
            fileinput.attr("accept",model.fileType());
        fileinput.attr("maxlength",model.maxlength());
        fileinput.attr("name",model.name());


        fileinput.css("width",(model.width() + 30)  + "px");
        fileinput.css("height", (model.height()) + "px");

        if (BrowserInfo.isIE8orLower()) {
            // make input invisible
            fileinput.css('filter', 'alpha(opacity=0)');
        }
        var list = model.list();
        if (list == undefined) {
            list = $("<div style='display:none'/>");
            button.append(list);
        }
        fileinput.MultiFile({
            list: list,
            onError: function(a,b,c,d) {
                var splittype = model.type().split(",")[0].split("/");
                var lasttype = splittype[splittype.length - 1].toUpperCase();
                var msg = $("<span>" + localization.onlyFiletypeAllowed + "</span>");
                msg.find('.put-filetype').text(lasttype);
                new FlashMessage({content: msg, type: 'error'});
                if (model.get('onError'))
                    model.get('onError')(a,b,c,d);
            },
            afterFileAppend : function(input,title,fileinput) {
                if (model.onAppend() != undefined)
                {
                    model.onAppend()(input,title,fileinput);
                    fileinput.n--;
                    _.each(fileinput.slaves, function(slave) {
                        $(slave).remove();
                    });
                    fileinput.slaves = [];
                    return false;
                }

                if (model.submitOnUpload()) {
                    if(model.get('showLoadingDialog'))
                        LoadingDialog.open({header: localization.loadingFile});
                    if (model.hasSubmit()) {
                        model.submit().addInputs(list);
                        model.submit().addInputs(input);
                        model.submit().send();
                    } else {
                        button.parents("form").submit();
                    }
                }
            }
        });
        button.append($("<span/>").append(fileinput)); // This span is is needed to fix some browser bug
        button.click(function() {return model.click()} );
        $(this.el).append(button);
        if (model.cssClass() != undefined)
          $(this.el).addClass(model.cssClass());
        return this;
    }
});

/* We add extra div here for some browsers compatibility */
window.UploadButton = function (args) {
          var model = new UploadButtonModel(args);
          var view = new UploadButtonView({model : model, el : $("<div style='position:relative;overflow:hidden;'/>")});
          this.el = function() {return $(view.el);};

};

});
