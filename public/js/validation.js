var Validation = Backbone.Model.extend({
    addCustomValidation: function(f, c, m) { 
        this.vlist.push({validates: f, callback: c, message: m}); 
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
                        this.vlist[i].callback(text, elem, this.vlist[i].message);

                    return false;
                }
            }

            return true;
    },
    fail: function(f, m) {
        for (var i in this.vlist) {
            if (typeof f == "function") this.vlist[i].callback = f;
            if (m) this.vlist[i].message = m;
        }
    }
});

//Here we need to define the initialize function because in javascript objects
//are shared by the reference. If we don't do this there will be always the
//same callbacks at all instances.
var NotEmptyValidation = Validation.extend({
    initialize: function(c, m) {
        this.vlist = [{
                validates: function(t) {
                    if (/^\s*$/.test(t))
                        return false;
                    
                    return true;
                }, 
                callback: c,
                message: m || "The value cannot be empty!"
        }];
    }
});

var EmailValidation = Validation.extend({
    initialize: function(c, m) {
        this.vlist = [{
            validates: function(t) {
                if (/^[\w._%+-]+@[\w.-]+[.][a-z]{2,4}$/i.test(t))
                    return true;
                return false;
            },
            callback: c,
            message: m || "Wrong email format!"
       }];
    }
});

var PasswordValidation = Validation.extend({
    initialize: function(c, m) {
        this.vlist = [{
            validates: function(t) {
                if (t.length < 8 || t.length > 250)
                    return false;
                
                if (!/([a-z]+[0-9]+)|([0-9]+[a-z]+)/.test(t))
                    return false;
                
                return true;
            },
            callback: c,
            message: m || "Password must contain one digit and one letter and must be 8 characters long at least!"
        }];
    }
});

var NameValidation = Validation.extend({
    initialize: function(c, m) {
        this.vlist = [{
            validates: function(t) {
                var t = $.trim(t);

                if (t.length == 0 || t.length > 100)
                    return false;

                if (/[^a-z-' ]/i.test(t))
                    return false;

                return true;
            },
            callback: c,
            message: m || "Name may contain only alphabetic characters, apostrophe, hyphen or space!"
        }];
    }
});

jQuery.fn.validate = function(validationObject){
    var validationObject = validationObject || (new NotEmptyValidation);
    var validates = true;

    this.each(function(){
            if (!validationObject.validate($(this).val(), $(this))) {
                validates = false;
                return false;
            }
        });

    return validates;
};

String.prototype.validate = function(validationObject) {
    var validationObject = validationObject || (new NotEmptyValidation);
    return validationObject.validate(this, this);
};

