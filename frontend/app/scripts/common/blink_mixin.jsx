module.exports = {
  _blinkTimeoutId: 0,

  getInitialState: function () {
    return {blinkCounter: 0};
  },

  blink: function (n, ms) {
    n = n || 5;
    ms = ms || 200;
    clearTimeout(this._blinkTimeoutId);
    this.setState({blinkCounter: n * 2});
    this._nextBlink(ms);
  },

  isHiddenByBlink: function () {
    return this.state.blinkCounter % 2 !== 0;
  },

  cancelBlink: function () {
    clearTimeout(this._blinkTimeoutId);
    this.setState({blinkCounter: 0});
  },

  _nextBlink: function (ms) {
    this._blinkTimeoutId = setTimeout(() => {
      let newBlinkCounter = this.state.blinkCounter - 1;
      this.setState({blinkCounter: newBlinkCounter});
      if (newBlinkCounter > 0) {
        this._nextBlink(ms);
      }
    }, ms);
  }
};
