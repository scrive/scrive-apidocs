var React = require("react");
var AccountSettingsPanel = require("../../../scripts/account/settings/accountsettingspanel");
var util = require("../../util");
var backend = require("../../backend");

var TestUtils = React.addons.TestUtils;

describe("scripts/account/settings/accountsettingspanel", function () {

  before(function () {
    server = backend.createServer();
  });

  it("Render account settings panel for company admin", function (done) {
    var panel = TestUtils.renderIntoDocument(React.createElement(AccountSettingsPanel, {
      companyAdmin: true
    }));
    util.waitUntil(function() {
      return panel.state.model.ready() && panel.refs.save != undefined;
    }, function(){
      var companysettings = panel.refs.companysettings;
      assert.ok(!companysettings.refs.companyname.props.readonly, "companyname is editable for company admin");
      assert.ok(!companysettings.refs.companynumber.props.readonly, "companynumber is editable for company admin");
      assert.ok(!companysettings.refs.companyaddress.props.readonly, "companyaddress is editable for company admin");
      assert.ok(!companysettings.refs.companyzip.props.readonly, "companyzip is editable for company admin");
      assert.ok(!companysettings.refs.companycity.props.readonly, "companycity is editable for company admin");
      assert.ok(!companysettings.refs.companycountry.props.readonly, "companycountry is editable for company admin");

      TestUtils.Simulate.click(panel.refs.save.getDOMNode());
      util.waitUntil(function() {
        return panel.state.model.ready() && panel.refs.save != undefined;
      }, function(){
        done();
      });
    });
  });

  it("Render account settings panel for company user", function (done) {
    var panel = TestUtils.renderIntoDocument(React.createElement(AccountSettingsPanel, {
      companyAdmin: false
    }));
    util.waitUntil(function() {
      return panel.state.model.ready() && panel.refs.save != undefined;
    }, function(){
      var companysettings = panel.refs.companysettings;
      assert.ok(companysettings.refs.companyname.props.readonly, "companyname is not editable for company admin");
      assert.ok(companysettings.refs.companynumber.props.readonly, "companynumber is not editable for company admin");
      assert.ok(companysettings.refs.companyaddress.props.readonly, "companyaddress is not editable for company admin");
      assert.ok(companysettings.refs.companyzip.props.readonly, "companyzip is not editable for company admin");
      assert.ok(companysettings.refs.companycity.props.readonly, "companycity is not editable for company admin");
      assert.ok(companysettings.refs.companycountry.props.readonly, "companycountry is not editable for company admin");

      TestUtils.Simulate.click(panel.refs.save.getDOMNode());
      util.waitUntil(function() {
        return panel.state.model.ready() && panel.refs.save != undefined;
      }, function(){
        done();
      });
    });
  });

  it("Change details", function (done) {
    var panel = TestUtils.renderIntoDocument(React.createElement(AccountSettingsPanel, {
      companyAdmin: true
    }));
    util.waitUntil(function() {
      return panel.state.model.ready() && panel.refs.save != undefined;
    }, function(){
      var accountsettings = panel.refs.accountsettings;
      var companysettings = panel.refs.companysettings;

      TestUtils.Simulate.change(accountsettings.refs.fstname.refs.input.getDOMNode(),{target: {value: "fstname"}});
      TestUtils.Simulate.change(accountsettings.refs.sndname.refs.input.getDOMNode(),{target: {value: "sndname"}});
      TestUtils.Simulate.change(accountsettings.refs.personalnumber.refs.input.getDOMNode(),{target: {value: "personalnumber"}});
      TestUtils.Simulate.change(accountsettings.refs.phone.refs.input.getDOMNode(),{target: {value: "phone"}});
      TestUtils.Simulate.change(accountsettings.refs.companyposition.refs.input.getDOMNode(),{target: {value: "companyposition"}});

      TestUtils.Simulate.change(companysettings.refs.companyname.refs.input.getDOMNode(),{target: {value: "companyname"}});
      TestUtils.Simulate.change(companysettings.refs.companynumber.refs.input.getDOMNode(),{target: {value: "companynumber"}});
      TestUtils.Simulate.change(companysettings.refs.companyaddress.refs.input.getDOMNode(),{target: {value: "companyaddress"}});
      TestUtils.Simulate.change(companysettings.refs.companyzip.refs.input.getDOMNode(),{target: {value: "companyzip"}});
      TestUtils.Simulate.change(companysettings.refs.companycity.refs.input.getDOMNode(),{target: {value: "companycity"}});
      TestUtils.Simulate.change(companysettings.refs.companycountry.refs.input.getDOMNode(),{target: {value: "companycountry"}});

      TestUtils.Simulate.click(panel.refs.save.getDOMNode());
      util.waitUntil(function() {
        return panel.state.model.ready() && panel.refs.save != undefined;
      }, function(){
        done();
      });
    });
  });

  it("Open change email modal", function (done) {
    var panel = TestUtils.renderIntoDocument(React.createElement(AccountSettingsPanel, {
      companyAdmin: true
    }));
    util.waitUntil(function() {
      return panel.state.model.ready() && panel.refs.save != undefined;
    }, function(){
      TestUtils.Simulate.click(panel.refs.accountsettings.refs.changeemail.getDOMNode());
      setTimeout(function() {
        util.cleanTimeoutsAndBody();
        done();
      },200);
    });
  });

  it("Open change password modal", function (done) {
    var panel = TestUtils.renderIntoDocument(React.createElement(AccountSettingsPanel, {
      companyAdmin: true
    }));
    util.waitUntil(function() {
      return panel.state.model.ready() && panel.refs.save != undefined;
    }, function(){

      TestUtils.Simulate.click(panel.refs.accountsettings.refs.changeemail.getDOMNode());
      setTimeout(function() {
        util.cleanTimeoutsAndBody();
        done();
      },200);
    });
  });

  it("Change language", function (done) {
    var panel = TestUtils.renderIntoDocument(React.createElement(AccountSettingsPanel, {
      companyAdmin: true
    }));
    util.waitUntil(function() {
      return panel.state.model.ready() && panel.refs.save != undefined;
    }, function(){
      // We need to simulate some page structure existing already
      var dummy = $("<div><div class='body-container'/><footer/></div>");
      $('body').append(dummy);
      TestUtils.Simulate.click(panel.refs.accountsettings.refs.languageselect.refs.select.getDOMNode());
      panel.refs.accountsettings.refs.languageselect.refs.select.select(0);
      TestUtils.Simulate.click(panel.refs.save.getDOMNode());
      setTimeout(function() {
        dummy.remove();
        util.cleanTimeoutsAndBody();
        done();
      },200);
    });
  });

  after(function () {
    util.cleanTimeoutsAndBody();
    server.restore();
  });

});


