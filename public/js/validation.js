
$(function(){
window.Validation = Backbone.Model.extend({
    defaults : {
        validates : function() {return true;},
        callback : function() {},
        message : "Validation has failed",
        next: undefined
    },
    concat: function(nextValidation) {
        if (this.get("next") == undefined)
            this.set({"next": nextValidation});
        else
            this.get("next").concat(nextValidation);
        return this;
    },
    validateData: function(text, elem) {
            if (!this.get("validates")(text)) {
               this.fail(text, elem);
               return false;       
            }
            else if (this.get("next") != undefined)
               return this.get("next").validateData(text,elem); 
            else
               return true; 
    },
    fail : function(text, elem) {
        if (this.get("callback") != undefined)
            return this.get("callback")(text, elem, this);
    },
    message: function() {
        return this.get("message");
    }
});

window.NotEmptyValidation = Validation.extend({
    defaults: {
           validates: function(t) {
                    if (/^\s*$/.test(t))
                        return false;
                    
                    return true;
            },
           message: "The value cannot be empty!"
        }
});

window.EmailValidation = Validation.extend({
     defaults: {
            validates: function(t) {
                if (/^[\w._%+-]+@[\w.-]+[.][a-z]{2,4}$/i.test(t))
                    return true;
                return false;
            },
            message: "Wrong email format!"
    }
});

window.PasswordValidation = Validation.extend({
    defaults: {
            validates: function(t) {
                if (t.length < 8 || t.length > 250)
                    return false;
                
                if (!/([a-z]+[0-9]+)|([0-9]+[a-z]+)/.test(t))
                    return false;
                
                return true;
            },
            message: "Password must contain one digit and one letter and must be 8 characters long at least!"
    }
});

window.NameValidation = Validation.extend({
    defaults: {
            validates: function(t) {
                var t = $.trim(t);

                if (t.length == 0 || t.length > 100)
                    return false;

                if (/[^a-z-' ]/i.test(t))
                    return false;

                return true;
            },
            message: "Name may contain only alphabetic characters, apostrophe, hyphen or space!"
    }
});

window.CheckboxReqValidation = Validation.extend({
    defaults: {
            validates: function(t) {
                return t.attr('checked');
            },
            message: "Checkbox must be checked!"
    }
});

window.DigitsLettersValidation = Validation.extend({
    defaults: {
        validates: function(t) {
            if (!/[a-z].*[a-z]/i.test(t))
                return false;
            if (!/[0-9].*[0-9]/.test(t))
                return false;

            return true;
        },
        message: "The field must have minimum two letters and two digits!"
    }
});

window.passwordValidation = function(callback, messages) {
    var messages = messages == undefined ? [] : messages;
    var message1 = messages[0] == undefined ? "" : messages[0];
    var message2 = messages[1] == undefined ? "" : messages[1];
    var message3 = messages[2] == undefined ? "" : messages[2];

    return new Validation({callback: callback, message: message1, validates: function(t) { return t.length >= 8;}})
               .concat(new Validation({callback: callback, message: message2, validates: function(t) { return t.length <= 250;}}))
               .concat(new DigitsLettersValidation({callback: callback, message: message3}));
}

jQuery.fn.validate = function(validationObject){
    var validationObject = validationObject || (new NotEmptyValidation);
    var validates = true;

    this.each(function(){
            //if this is a checkbox then passing value makes no sense for validation
            if ($(this).attr('type') == 'checkbox') {
                if (!validationObject.validateData($(this), $(this))) {
                    validates = false;
                    return false; 
                } 
            } else if (!validationObject.validateData($(this).val(), $(this))) {
                validates = false;
                return false;
            }
        });

    return validates;
};

String.prototype.validate = function(validationObject) {
    var validationObject = validationObject || (new NotEmptyValidation);
    return validationObject.validateData(this);
};

});
