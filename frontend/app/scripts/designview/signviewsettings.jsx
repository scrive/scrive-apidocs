/** @jsx React.DOM */

define(['React','Backbone', 'common/backbone_mixin', 'common/checkbox', 'themes/theme', 'themes/previews/signing' ,'legacy_code'], function(React, Backbone, BackboneMixin, Checkbox, Theme, SignviewPreview) {

var SignviewSettingsModel = Backbone.Model.extend({
  initialize: function(args) {
    var self = this;
    var theme = new Theme({url: "/account/company/companybranding/signviewtheme"});
    theme.bind("change", function() {
      self.trigger("change");
    });
    this.set({
      theme: theme,
      showHeader: args.document.showheader() == undefined ? true : args.document.showheader(),
      showRejectOption: args.document.showrejectoption() == undefined ? true : args.document.showrejectoption(),
      showPDFDownload: args.document.showpdfdownload() == undefined ? true : args.document.showpdfdownload(),
      showFooter: args.document.showfooter() == undefined ? true : args.document.showfooter(),
    });
    theme.fetch({cache: false });
  },
  document: function() {
     return this.get("document");
  },
  theme: function() {
     return this.get("theme");
  },
  showHeader: function() {
     return this.get("showHeader");
  },
  setShowHeader: function(b) {
     return this.set("showHeader",b);
  },
  showRejectOption: function() {
    return this.get("showRejectOption");
  },
  setShowRejectOption: function(b) {
     return this.set("showRejectOption",b);
  },
  showPDFDownload: function() {
    return this.get("showPDFDownload");
  },
  setShowPdfDownload: function(b) {
     return this.set("showPDFDownload",b);
  },
  showFooter: function() {
    return this.get("showFooter");
  },
  setShowFooter: function(b) {
     return this.set("showFooter",b);
  },
  ready : function() {
    return this.document().ready() && this.theme().ready();
  }
});


var SignviewSettingsView = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    render: function() {
      var self = this;
      var model = self.props.model;
      if (!model.ready()) {
        return (<div/>)
      } else {
        return (
          <div className="signviewsettings">
            <div className="options">
              <Checkbox
                checked={model.showHeader()}
                label={localization.designview.signviewsettings.showheader}
                onChange={function(c) {model.setShowHeader(c);}}
              />
              <Checkbox
                checked={model.showRejectOption()}
                label={localization.designview.signviewsettings.showrejectoption}
                onChange={function(c) {model.setShowRejectOption(c);}}
              />
              <Checkbox
                checked={model.showPDFDownload()}
                label={localization.designview.signviewsettings.showpdfdownload}
                onChange={function(c) {model.setShowPdfDownload(c);}}
              />
              <Checkbox
                checked={model.showFooter()}
                label={localization.designview.signviewsettings.showfooter}
                onChange={function(c) {model.setShowFooter(c);}}
              />
            </div>
            <div className="container">
              <SignviewPreview
                model={model.theme()}
              />
            </div>
          </div>
        );
      }
    }
});

return function(args) {
  var document = args.document;
  var model = new SignviewSettingsModel({document : document});
  var settingsView = $("<div/>");
  React.renderComponent(SignviewSettingsView({
    model: model
  }), settingsView[0]);
  var popup = new Confirmation({
    content: settingsView,
    title: localization.designview.signviewsettings.title,
    icon: undefined,
    acceptText: localization.save,
    width: 900,
    onAccept : function() {
      document.setShowheader(model.showHeader());
      document.setShowrejectoption(model.showRejectOption());
      document.setShowpdfdownload(model.showPDFDownload());
      document.setShowfooter(model.showFooter());
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
};

});

