var BlinkGroupModel = require(
  "../../../scripts/signview/navigation/blinkgroupmodel"
);

describe("signview/navigation/blinkgroupmodel", function () {
  beforeEach(function () {
    sinon.stub(window, "setTimeout").returns(1);
    sinon.stub(window, "clearTimeout");
  });

  afterEach(function () {
    window.setTimeout.restore();
    window.clearTimeout.restore();
  });

  it("should initialize defaults", function () {
    var model = new BlinkGroupModel();
    assert.isNull(model.get("groupId"));
    assert.equal(model.get("counter"), 0);
    assert.equal(model.get("interval"), 0);
    assert.equal(model.get("timeoutId"), 0);
  });

  it("should not start blinking if it already is", function () {
    var model = new BlinkGroupModel({timeoutId: 1});
    sinon.stub(model, "set");

    model.blink();
    assert.isFalse(model.set.called);
  });

  it("should start blinking", function () {
    var model = new BlinkGroupModel();
    sinon.stub(model, "set");

    model.blink(10, 300);

    assert(window.setTimeout.called);
    var setTimeoutArgs = window.setTimeout.firstCall.args;
    assert.isFunction(setTimeoutArgs[0]);
    assert.equal(setTimeoutArgs[1], 300);

    assert.isTrue(model.set.calledWith({
      counter: 20,
      interval: 300,
      timeoutId: 1
    }));
  });

  it("should use defaults when starting blinking", function () {
    var model = new BlinkGroupModel();
    sinon.stub(model, "set");

    model.blink();

    assert(window.setTimeout.called);
    var setTimeoutArgs = window.setTimeout.firstCall.args;
    assert.isFunction(setTimeoutArgs[0]);
    assert.equal(setTimeoutArgs[1], 200);

    assert.isTrue(model.set.calledWith({
      counter: 10,
      interval: 200,
      timeoutId: 1
    }));
  });

  it("should not cancel blinking if it isn't blinking", function () {
    var model = new BlinkGroupModel();
    sinon.stub(model, "set");

    model.cancelBlink();
    assert.isFalse(window.clearTimeout.called);
    assert.isFalse(model.set.called);
  });

  it("should cancel blinking", function () {
    var model = new BlinkGroupModel({timeoutId: 1});
    sinon.stub(model, "set");

    model.cancelBlink();
    assert.isTrue(window.clearTimeout.calledWith(1));
    assert.isTrue(model.set.calledWith({
      counter: 0,
      timeoutId: 0
    }));
  });

  it("should end blinking", function () {
    var model = new BlinkGroupModel({counter: 1});
    sinon.stub(model, "cancelBlink");
    sinon.stub(model, "set");

    model.onBlinkTimeout();
    assert.isTrue(model.cancelBlink.called);
    assert.isFalse(model.set.called);
  });

  it("should blink when timeout callback is fired", function () {
    var model = new BlinkGroupModel({counter: 2, interval: 300});
    sinon.stub(model, "cancelBlink");
    sinon.stub(model, "set");

    model.onBlinkTimeout();
    assert.isFalse(model.cancelBlink.called);

    assert(window.setTimeout.called);
    var setTimeoutArgs = window.setTimeout.firstCall.args;
    assert.isFunction(setTimeoutArgs[0]);
    assert.equal(setTimeoutArgs[1], 300);

    assert.isTrue(model.set.calledWith({
      counter: 1,
      timeoutId: 1
    }));
  });
});
