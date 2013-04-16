(function(window) {

var monthName = function (v) {
 switch(v)
  {
  case 0:
    return localization.dates.jan
  case 1:
    return localization.dates.feb
  case 2:
    return localization.dates.mar
  case 3:
    return localization.dates.apr
  case 4:
    return localization.dates.may
  case 5:
    return localization.dates.jun
  case 6:
    return localization.dates.jul
  case 7:
    return localization.dates.aug
  case 8:
    return localization.dates.sep
  case 9:
    return localization.dates.oct
  case 10:
    return localization.dates.nov
  case 11:
    return localization.dates.dec
  default:
    return ""
  }
}

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

if (!Date.prototype.toTimeAbrev) {
    Date.prototype.toTimeAbrev = function() {
        var curr = new Date();
        function pad(n) { return n < 10 ? '0' + n : n }

        if (this.getUTCFullYear() == curr.getUTCFullYear() && this.getUTCMonth() == curr.getUTCMonth() && this.getUTCDate() == curr.getUTCDate())
          return pad(this.getHours() + 1) + ":" + pad(this.getMinutes() + 1);
        if (this.getUTCFullYear() == curr.getFullYear())
          return this.getDate() + " " + monthName(this.getMonth())
        else
          return this.getFullYear() + "-" + pad(this.getMonth() + 1) + "-" + pad(this.getDate());
    };
}

if (!Date.prototype.fullTime) {
    Date.prototype.fullTime = function() {
        var curr = new Date();
        function pad(n) { return n < 10 ? '0' + n : n }
        return this.getFullYear() + "-" + pad(this.getMonth() + 1) + "-" + pad(this.getDate()) + " " + pad(this.getHours() + 1) + ":" + pad(this.getMinutes() + 1) + ":" + pad(this.getSeconds())

    };
}



})(window);