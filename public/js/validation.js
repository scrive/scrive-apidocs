var Validation = Backbone.Model.extend({
    addCustomValidation: function(f, c) { 
        var c = c || this.vlist[this.vlist.length - 1].callback; 
        this.vlist.push({validates: f, callback: c}); 
    },
    concat: function(validateObjList) { 
            var newVList = [];
            newVList = newVList.concat(this.vlist);

            if (typeof validateObjList == "array")
                for (var i in validateObjList) 
                    newVList = newVList.concat(validateObjList[i].vlist);
            else if (validateObjList.vlist)
                newVList = newVList.concat(validateObjList.vlist);

            //there's always only one instance so we don't need to define
            //initialize function
            var ValidationObject = Validation.extend({vlist: newVList}); 

            return new ValidationObject;
    },
    validate: function(text, elem) {
            for (var i in this.vlist) {
                if (!this.vlist[i].validates(text)) {
                    if (this.vlist[i].callback)
                        this.vlist[i].callback(text, elem);

                    return false;
                }
            }

            return true;
    },
    fail: function(f) {
        if (typeof f == "function") {
            for (var i in this.vlist) {
                this.vlist[i].callback = f;
            }
        }
    }
});

//Here we need to define the initialize function because in javascript objects
//are shared by the reference. If we don't do this there will be always the
//same callbacks at all instances.
var NotEmptyValidation = Validation.extend({
        initialize: function() {
            this.vlist = [{
                    validates: function(t) {
                        if (!t || t == "")
                            return false;

                        if (t.length != undefined && t.length == 0)
                            return false;
                        
                        return true;
                    }, 
                    callback: function(t,e) {}
                }];
        }
    });

var EmailFormatValidation = Validation.extend({
        initialize: function() {
            this.vlist = [{
                    validates: function(t) {
                        if (/^[\w._%+-]+@[\w.-]+[.][a-z]{2,4}$/i.test(t))
                            return true;
                        return false;
                    },
                    callback: function(t,e) {}
               }];
        }
    });

jQuery.fn.validate = function(validationObject){
    var validationObject = validationObject || (new NotEmptyValidation);

    return this.each(function(){
            if (!validationObject.validate($(this).val(), $(this))) {
                return false;
            }
        });
};

String.prototype.validate = function(validationObject) {
    var validationObject = validationObject || (new NotEmptyValidation);
    return validationObject.validate(this, this);
};

