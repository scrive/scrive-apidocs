// Some trivial test of the common React components.

define(["legacy_code", "React", "common/daysinputwithcalendar"], function(undefined, React, DaysInputWithCalendar
) {

  var TestUtils = React.addons.TestUtils;

  describe("common/daysinputwithcalendar", function () {
    it("should test the DaysInputWithCalendar renders correctly and calendar acts as expected", function () {
      assert.equal($('#calroot').size(),0); //Callendar should not be initiated
      var daysInputWithCalendar = TestUtils.renderIntoDocument(DaysInputWithCalendar({
        infotext        : "-",
        label           : "days",
        className       : "main-class",
        inputClassName  : "input-class",
        labelClassName  : "label-class",
        days            : 10,
        minDays         : 1,
        maxDays         : 60,
        canBeEmpty      : true,
        onChange        : function() {}
      }));
      assert.equal($('#calroot').size(), 1); //Calendar should be initiated
      assert.equal($('#calroot').css("display"),"none"); //Calendar should not be visible

      assert.ok($(daysInputWithCalendar.getDOMNode()).hasClass("main-class"));
      var label = TestUtils.findRenderedDOMComponentWithClass(daysInputWithCalendar, "label-class");
      assert.equal($(label.getDOMNode()).text(),"days");

      var infoTextInput = daysInputWithCalendar.refs.input;
      assert.equal(infoTextInput.value(), "10");
      var input = TestUtils.findRenderedDOMComponentWithTag(daysInputWithCalendar, "input");
      assert.equal($(input.getDOMNode()).attr("placeholder"),"-");
      // Calendar should also be rendered on page
      var calendarbutton = TestUtils.findRenderedDOMComponentWithClass(daysInputWithCalendar, "calendarbutton");
      $(calendarbutton.getDOMNode()).click(); // Can't use TestUtils, since calendar is build with jQuery, and will not respond to TestUtils.Simulate
      assert.equal($('#calroot').css("display"),"block"); //Calendar should be visible
      $('body').click() //After clicking on something else, calendar should be gone
      assert.equal($('#calroot').css("display"),"none");
    });

    it("should test the DaysInputWithCalendar respects max and mix days", function () {
      var currentDaysValue = 10;
      var daysInputWithCalendar = TestUtils.renderIntoDocument(DaysInputWithCalendar({
        infotext        : "-",
        label           : "days",
        className       : "main-class",
        inputClassName  : "input-class",
        labelClassName  : "label-class",
        days            : 10,
        minDays         : 2,
        maxDays         : 60,
        canBeEmpty      : false,
        onChange        : function(v) {currentDaysValue  = v;}
      }));

      var input = TestUtils.findRenderedDOMComponentWithTag(daysInputWithCalendar, "input");
      TestUtils.Simulate.change(input.getDOMNode(),{target: {value: "aaaa"}});
      assert.equal(currentDaysValue,10);
      assert.equal($(input.getDOMNode()).val(),"10"); // When typing something wrong we should show valid number
      TestUtils.Simulate.change(input.getDOMNode(),{target: {value: ""}});
      assert.equal(currentDaysValue,10);
      assert.equal($(input.getDOMNode()).val(),""); // But we should leave empty value
      TestUtils.Simulate.blur(input.getDOMNode());
      assert.equal($(input.getDOMNode()).val(),"10"); // Until someone will blur. Then we fallback to current value
      TestUtils.Simulate.change(input.getDOMNode(),{target: {value: "70"}});
      assert.equal(currentDaysValue,60);
      TestUtils.Simulate.change(input.getDOMNode(),{target: {value: "-2"}});
      assert.equal(currentDaysValue,2);
      TestUtils.Simulate.change(input.getDOMNode(),{target: {value: "12"}});
      assert.equal(currentDaysValue,12);
    });

  });
});
