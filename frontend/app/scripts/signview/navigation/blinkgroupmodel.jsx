var Backbone = require("backbone");

var DEFAULT_COUNT = 5;
var DEFAULT_INTERVAL = 200;

var BlinkGroupModel = Backbone.Model.extend({
  defaults: {
    groupId: null,
    counter: 0,
    interval: 0,
    timeoutId: 0
  },
  blink: function (count, interval) {
    if (!this.get("timeoutId")) {
      interval = interval || DEFAULT_INTERVAL;
      count = count || DEFAULT_COUNT;

      this.set({
        counter: count * 2,
        interval: interval,
        timeoutId: window.setTimeout(this.onBlinkTimeout.bind(this), interval)
      });
    }
  },
  cancelBlink: function () {
    var timeoutId = this.get("timeoutId");
    if (timeoutId) {
      window.clearTimeout(timeoutId);
      this.set({counter: 0, timeoutId: 0});
    }
  },
  onBlinkTimeout: function () {
    var newCounter = this.get("counter") - 1;

    if (newCounter > 0) {
      this.set({
        counter: newCounter,
        timeoutId: window.setTimeout(
          this.onBlinkTimeout.bind(this), this.get("interval")
        )
      });
    } else {
      this.cancelBlink();
    }
  }
});

module.exports = BlinkGroupModel;
