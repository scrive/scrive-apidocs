define(['Backbone', 'legacy_code'], function() {

var monthShortName = function (v) {
 switch(v)
  {
  case 0:
    return localization.dates.jan;
  case 1:
    return localization.dates.feb;
  case 2:
    return localization.dates.mar;
  case 3:
    return localization.dates.apr;
  case 4:
    return localization.dates.may;
  case 5:
    return localization.dates.jun;
  case 6:
    return localization.dates.jul;
  case 7:
    return localization.dates.aug;
  case 8:
    return localization.dates.sep;
  case 9:
    return localization.dates.oct;
  case 10:
    return localization.dates.nov;
  case 11:
    return localization.dates.dec;
  default:
    return "";
  }
};

var monthFullName = function (v) {
 switch(v)
  {
  case 0:
    return localization.dates.january;
  case 1:
    return localization.dates.february;
  case 2:
    return localization.dates.march;
  case 3:
    return localization.dates.april;
  case 4:
    return localization.dates.mayy;
  case 5:
    return localization.dates.june;
  case 6:
    return localization.dates.july;
  case 7:
    return localization.dates.august;
  case 8:
    return localization.dates.september;
  case 9:
    return localization.dates.october;
  case 10:
    return localization.dates.november;
  case 11:
    return localization.dates.december;
  default:
    return "";
  }
};

if (!Date.prototype.toISOString) {
    Date.prototype.toISOString = function() {
        function pad(n) { return n < 10 ? '0' + n : n }
        return this.getUTCFullYear() + '-'
            + pad(this.getUTCMonth() + 1) + '-'
            + pad(this.getUTCDate()) + 'T'
            + pad(this.getUTCHours()) + ':'
            + pad(this.getUTCMinutes()) + ':'
            + pad(this.getUTCSeconds()) + 'Z';
    };
}

if (!Date.prototype.toYMDString) {
    Date.prototype.toYMDString = function() {
        function pad(n) { return n < 10 ? '0' + n : n } // refactor
        return this.getUTCFullYear() + '-'
         + pad(this.getUTCMonth() + 1) + '-'
         + pad(this.getUTCDate());
    };
}

if (!Date.prototype.toTimeAbrev) {
    Date.prototype.toTimeAbrev = function() {
        var curr = new Date();
        function pad(n) { return n < 10 ? '0' + n : n }

        if (this.getUTCFullYear() == curr.getUTCFullYear() && this.getUTCMonth() == curr.getUTCMonth() && this.getUTCDate() == curr.getUTCDate())
          return pad(this.getHours()) + ":" + pad(this.getMinutes());
        if (this.getUTCFullYear() == curr.getFullYear())
          return this.getDate() + " " + monthShortName(this.getMonth());
        else
          return this.getFullYear() + "-" + pad(this.getMonth() + 1) + "-" + pad(this.getDate());
    };
}

if (!Date.prototype.toTimeAbrevWithMinutes) {
    Date.prototype.toTimeAbrevWithMinutes = function() {
        var curr = new Date();
        function pad(n) { return n < 10 ? '0' + n : n }
        var minutesPart = pad(this.getHours()) + ":" + pad(this.getMinutes());

        if (this.getUTCFullYear() == curr.getUTCFullYear() && this.getUTCMonth() == curr.getUTCMonth() && this.getUTCDate() == curr.getUTCDate())
          return minutesPart;
        if (this.getUTCFullYear() == curr.getFullYear())
          return this.getDate() + " " + monthShortName(this.getMonth()) + " " + minutesPart;
        else
          return this.getFullYear() + "-" + pad(this.getMonth() + 1) + "-" + pad(this.getDate()) + " " + minutesPart;
    };
}

if (!Date.prototype.fullTime) {
    Date.prototype.fullTime = function() {
        function pad(n) { return n < 10 ? '0' + n : n }
        return this.getDate() + " " + monthFullName(this.getMonth()) + " " +  this.getFullYear() + " " + pad(this.getHours()) + ":" + pad(this.getMinutes());

    };
}

if (!Date.prototype.diffDays) {
    Date.prototype.diffDays = function(date2) {
    var time2 = 0;
    if (date2 != undefined)
        time2 = date2.getTime();
    else
        time2 = new Date().getTime() +1; // This +1 removes a risk that diffDays depends on speed of browser.
    var ONE_DAY = 1000 * 60 * 60 * 24;
    var difference_ms = Math.abs(this.getTime() - time2);
    return Math.floor(difference_ms / ONE_DAY);
    };
}

});
