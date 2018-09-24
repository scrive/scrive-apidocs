var Backbone = require("backbone");
var Validation = require("./validation.js").Validation;
var $ = require("jquery");
var _ = require("underscore");
var DigitsLettersValidation = require("./validation.js").DigitsLettersValidation;
var NotEmptyValidation = require("./validation.js").NotEmptyValidation;
var defs = require("./defs");


var Validation = exports.Validation = Backbone.Model.extend({
    defaults : {
        validates : function() {return true;},
        callback : function() {},
        message : "Validation has failed"
    },
    concat: function(nextValidation) {
        if (this.get("next") == undefined)
            this.set({"next": nextValidation});
        else
            this.get("next").concat(nextValidation);
        return this;
    },
    validateData: function(text, elem, skipCallback) {
      var validates = this.get("validates");
      var alternative = this.get("alternative");
      var next = this.get("next");
      if (!validates.call(this, text)) {
        if (alternative !== undefined) {
          return alternative.validateData(text, elem);
        }
        if (skipCallback !== true) {
          this.fail(text, elem);
        }
        return false;
      } else if (next !== undefined) {
        if (alternative === undefined) {
          return next.validateData(text, elem);
        } else {
          return next.validateData(text, elem, true) || alternative.validateData(text, elem);
        }
      }
      return true;
    },
    or: function(alternativeValidation) {
      var currentAlternative = this.get("alternative");
      if (currentAlternative === undefined) {
        this.set({"alternative": alternativeValidation});
      } else {
        currentAlternative.or(alternativeValidation);
      }
      return this;
    },
    fail : function(text, elem) {
        if (this.get("callback") != undefined)
            return this.get("callback")(text, elem, this);
    },
    message: function() {
        return this.get("message");
    },
    setMessage: function(msg) {
      this.set('message', msg);
    },
    setCallback : function(callback) {
        this.set({callback : callback});
        if (this.get('next') != undefined) this.get('next').setCallback(callback);
        return this;
    }
});

var NoValidation = exports.NoValidation = Validation.extend({
    defaults: {
           validates: function(t) { return true; },
           message: "The value should be always ok"
        }
});

var EmptyValidation = exports.EmptyValidation = Validation.extend({
    defaults: {
      validates: function(t) {
        return /^\s*$/.test(t);
      },
      message: "The value must be empty!"
    }
});

var NotEmptyValidation = exports.NotEmptyValidation = Validation.extend({
    defaults: {
           validates: function(t) {

                    if (/^\s*$/.test(t)) // only spaces
                        return false;

                    return true;
            },
           message: "The value cannot be empty!"
        }
});

var EmailValidation = exports.EmailValidation = Validation.extend({
     defaults: {
            validates: function(t) {
                t = t || "";
                t = t.trim();
                // this does allow international characters only in domain, which for the moment is good
                var regex = new RegExp(defs.PATTERN_EMAIL, "i");
                if (regex.test(t))
                    return true;
                return false;
            },
            message: "Wrong email format!"
    }
});

var PhoneValidation = exports.PhoneValidation = Validation.extend({
     defaults: {
            validates: function(t) {
                /* After trimming and removal of ignorable characters
                 * mobile phone number is required to have 11 digits
                 * including country code. Prefix it with plus sign.
                 */
                var z = t.replace(/-/g, "").replace(/\s/g, "");
                var regex = new RegExp(defs.PATTERN_PHONE, "i");
                return regex.test(z);
            },
            message: "Wrong phone number format!"
    }
});

var PhoneValidationNO = exports.PhoneValidationNO = Validation.extend({
     defaults: {
            validates: function(t) {
              // Norwegian phone numbers +47xxxxxxxx.
              return /^\+47[0-9]{8}$/.test(t);
            },
            message: "Wrong Norwegian phone number format!"
    }
});

// we want to match international characters
// http://stackoverflow.com/questions/1073412/javascript-validation-issue-with-international-characters
// u0021-007E is ASCII special characters (see http://www.unicode.org/charts/PDF/U0000.pdf)
// If we want to match more see: http://www.unicode.org/charts/
var nameCharactersRegex = /[^a-z\u0021-\u007E\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF-' ]/;

var NameValidation = exports.NameValidation = Validation.extend({
    defaults: {
            validates: function(t) {
                var t = $.trim(t);

                if (t.length === 0 || t.length > 100) {
                    return false;
                }
                if (nameCharactersRegex.test(t)) {
                    return false;
                }

                return true;
            },
            message: "Name may contain only alphabetic characters, apostrophe, hyphen or space!"
    }
});

var UserNameValidation = exports.UserNameValidation = Validation.extend({
    initialize: function(args) {
      this.set('firstName', args.firstName);
      this.set('lastName', args.lastName);
    },
    firstName: function() {
      return this.get('firstName');
    },
    lastName: function() {
      return this.get('lastName');
    },
    makeCopyForField: function(fieldName, copyText) {
      var res = $("<span>" + copyText + "<span/>");
      $('.put-field-name',res).text(fieldName);
      return res.html();
    },
    defaults: {
            validates: function(t) {
                var words = _.filter(t.split(' '), function(x) { return x.trim() != '';});
                var firstName = words[0];
                if (firstName === undefined) {
                  firstName = '';
                }

                var lastName = _.rest(words).join(' ');

                if (firstName.length === 0) {
                    var text = this.makeCopyForField(this.firstName(), localization.validation.required);
                    this.setMessage(text);
                    return false;
                }
                if (firstName.length > 100) {
                    var text = this.makeCopyForField(this.firstName(), localization.validation.toolong);
                    this.setMessage(text);
                    return false;
                }
                if (lastName.length === 0) {
                    var text = this.makeCopyForField(this.lastName(), localization.validation.required);
                    this.setMessage(text);
                    return false;
                }
                if (lastName.length > 100) {
                    var text = this.makeCopyForField(this.lastName(), localization.validation.toolong);
                    this.setMessage(text);
                    return false;
                }
                if (nameCharactersRegex.test(firstName)) {
                    var text = this.makeCopyForField(this.firstName(), localization.validation.invalidnamechars);
                    this.setMessage(text);
                    return false;
                }
                if (nameCharactersRegex.test(lastName)) {
                    var text = this.makeCopyForField(this.lastName(), localization.validation.invalidnamechars);
                    this.setMessage(text);
                    return false;
                }

                return true;
            }
    }
});

var CheckboxReqValidation = exports.CheckboxReqValidation = Validation.extend({
    defaults: {
            validates: function(t) {
                return t.attr('checked') || t.hasClass("checked");
            },
            message: "Checkbox must be checked!"
    }
});

var DigitsLettersValidation = exports.DigitsLettersValidation = Validation.extend({
    defaults: {
        validates: function(t) {
            // we don't allow international in password; good or bad?
            if (!/[a-z]/i.test(t))
                return false;
            if (!/[0-9]/.test(t))
                return false;

            return true;
        },
        message: "The field must have minimum one letter and one digits!"
    }
});

var NumberValidation = exports.NumberValidation = Validation.extend({
    defaults: {
        validates: function(t) {
           return /^[0-9]{1,}$/i.test(t);
        },
        message: "The field must contain only digitas"
    }
});

var PasswordValidation = exports.PasswordValidation = Validation.extend({
    defaults: {
        validates: function(t) { return t.length >= 8; },
        message: "Password must contain 8 characters at least!",
        message_max: "Password must contain 250 characters at most!",
        message_digits: "Password must have minimum one digits and one letters!"
    },
    initialize: function() {
        this.set({"next": new Validation({
            callback: this.get("callback"),
            message: this.get("message_max"),
            validates: function(t) { return t.length <= 250; }})});

        this.concat(new DigitsLettersValidation({
            callback: this.get("callback"),
            message: this.get("message_digits")
        }));

    }
});

var PasswordEqValidation = exports.PasswordEqValidation = Validation.extend({
    defaults: {
        validates: function(t) {
            var p1 = this.get("with") ? this.get("with")(): undefined;

            return t == p1;
        },
        message: "The two passwords don't match!"
    }
});

var SSNForSEBankIDValidation = exports.SSNForSEBankIDValidation = Validation.extend({
    defaults: {
        validates: function(t) {
           var fWithoutHyphens = t.replace(/\+|-/g, "");
           return /^([0-9]{10}|[0-9]{12})$/i.test(fWithoutHyphens);
        },
        message: "Personal number for Swedish BankID must contain 10 or 12 digits"
    }
});

var SSNForNOBankIDValidation = exports.SSNForNOBankIDValidation = Validation.extend({
    defaults: {
        validates: function(t) {
           var fWithoutHyphens = t.replace(/-/g, "");
           return /^([0-9]{11})$/i.test(fWithoutHyphens);
        },
        message: "Personal number for Norwegian BankID must contain 11 digits"
    }
});

var SSNForDKNemIDValidation = exports.SSNForDKNemIDValidation = Validation.extend({
    defaults: {
        validates: function(t) {
           var fWithoutHyphens = t.replace(/-/g, "");
           return /^([0-9]{10})$/i.test(fWithoutHyphens);
        },
        message: "Personal number for Danish NemID must contain 10 digits"
    }
});

var SSNForFITupasValidation = exports.SSNForFITupasValidation = Validation.extend({
    defaults: {
        validates: function(t) {
           // Format: DDMMYY [+-A] XXX checksumCharacter
           try {
             var result = t.toUpperCase().match(/^([0-9]{2})([0-9]{2})([0-9]{2})[+-A][0-9]{3}[0123456789ABCDEFHJKLMNPRSTUVWXY]$/)
             var checksumChars = "0123456789ABCDEFHJKLMNPRSTUVWXY"
             var day = Number(result[1])
             var month = Number(result[2])
             var year = Number(result[3])
             var computedChecksumIdx = Number(result[0].slice(0,6) + result[0].slice(7,10)) % (checksumChars.length)
             var computedChecksum = checksumChars[computedChecksumIdx]
             return (1 <= day && day <= 31 && 1 <= month && month <= 12 && computedChecksum == result[0][10])
           }
           catch(err) {
             console.log(err);
             return false;
           }
        },
        message: "Personal number for Finnish TUPAS must be in format DDMMYY?XXXC"
    }
});

jQuery.fn.validate = function(validationObject){
    var validationObject = validationObject || (new NotEmptyValidation);
    var validates = true;

    this.each(function(){
            //if this is a checkbox then passing value makes no sense for validation
            if ($(this).attr('type') == 'checkbox' || $(this).hasClass('checkbox')) {
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
