/* This is component for configuring signview settings, such as
 * hiding the top bar or the reject button for this document.
 */
define(['Backbone', 'React', 'common/checkbox', 'legacy_code'], function(Backbone, React, Checkbox) {

var SignviewSettingsModel = Backbone.Model.extend({
  initialize: function(args) {
    this.set({
      showHeader: new SignviewSettingsBoolean({
        checked: args.document.showheader() == undefined ? true : args.document.showheader(),
        label: localization.designview.signviewsettings.showheader
      }),
      showRejectOption: new SignviewSettingsBoolean({
        checked: args.document.showrejectoption() == undefined ? true : args.document.showrejectoption(),
        label: localization.designview.signviewsettings.showrejectoption
      }),
      showPDFDownload: new SignviewSettingsBoolean({
        checked: args.document.showpdfdownload() == undefined ? true : args.document.showpdfdownload(),
        label: localization.designview.signviewsettings.showpdfdownload
      }),
      showFooter: new SignviewSettingsBoolean({
        checked: args.document.showfooter() == undefined ? true : args.document.showfooter(),
        label: localization.designview.signviewsettings.showfooter
      })
    }, {silent: true});
  },
  showHeader: function() { return this.get("showHeader"); },
  showRejectOption: function() { return this.get("showRejectOption"); },
  showPDFDownload: function() { return this.get("showPDFDownload"); },
  showFooter: function() { return this.get("showFooter"); }
});

var SignviewSettingsView = Backbone.View.extend({
    initialize: function (args) {
      this.model = args.model;
      this.document = args.document;
      this.companyui = args.companyui;
      this.render();
    },
    render: function() {
      var view = new SampleSignView({
        signviewbranding: new CompanyBrandingSignViewModel({companyui: this.companyui}),
        signviewsettings: this.model
      });

      var container = $('<div class="signviewsettings" />');

      var signviewcontainer = $('<div class="container" />');
      signviewcontainer.append($(view.el()));

      var options = $('<div class="options" />');
      options.append(this.model.showHeader().el());
      options.append(this.model.showPDFDownload().el());
      options.append(this.model.showRejectOption().el());
      options.append(this.model.showFooter().el());

      container.append(options);
      container.append(signviewcontainer);

      this.$el.empty().append(container);
    }
});

window.SignviewSettingsPopup = function(args) {
  var document = args.document;
  var model = new SignviewSettingsModel({document : document});
  var companyui = new CompanyUI({url: "/account/company/json"});

  companyui.bind("change:ready", function() {
    if (companyui.ready()) {
      var settingsView = new SignviewSettingsView({model: model, companyui: companyui});
      var popup = new Confirmation({
        content: settingsView.el,
        title: localization.designview.signviewsettings.title,
        icon: undefined,
        acceptText: localization.save,
        width: 900,
        onAccept : function() {
          document.setShowheader(model.showHeader().checked());
          document.setShowrejectoption(model.showRejectOption().checked());
          document.setShowpdfdownload(model.showPDFDownload().checked());
          document.setShowfooter(model.showFooter().checked());
          if (args.onClose !== undefined) {
            args.onClose();
          }
          return true;
        },
        onReject: function() {
          if (args.onClose !== undefined) {
            args.onClose();
          }
        }
      });
    }
  });
};

var SignviewSettingsBoolean = function(args) {
  var div = $('<div />');
  var checkbox = React.renderComponent(Checkbox({
    initiallyChecked: args.checked,
    label: args.label
  }), div[0]);

  return {
    checked: function() { return checkbox.state.checked; },
    onChange: function(f) { checkbox.props.onChange = function(checked) { f(checked); }; },
    el: function() { return div; }
  };
};

});

