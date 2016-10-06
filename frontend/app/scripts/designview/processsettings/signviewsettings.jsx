var React = require("react");
var Backbone = require("backbone");
var BackboneMixin = require("../../common/backbone_mixin");
var Checkbox = require("../../common/checkbox");
var Theme = require("../../themes/theme");
var SignviewPreview = require("../../themes/previews/signing");
var $ = require("jquery");
var Modal = require("../../common/modal");

var SignviewSettingsModel = Backbone.Model.extend({
  initialize: function (args) {
    var self = this;
    var theme = new Theme({url: "/account/company/companybranding/signviewtheme"});
    theme.bind("change", function () {
      self.trigger("change");
    });
    this.set({
      theme: theme,
      showHeader: args.document.showheader() == undefined ? true : args.document.showheader(),
      showRejectOption: args.document.showrejectoption() == undefined ? true : args.document.showrejectoption(),
      allowRejectReason: args.document.allowrejectreason() == undefined ? true : args.document.allowrejectreason(),
      showPDFDownload: args.document.showpdfdownload() == undefined ? true : args.document.showpdfdownload(),
      showFooter: args.document.showfooter() == undefined ? true : args.document.showfooter()
    });
    theme.fetch({cache: false});
  },
  document: function () {
    return this.get("document");
  },
  theme: function () {
    return this.get("theme");
  },
  showHeader: function () {
    return this.get("showHeader");
  },
  setShowHeader: function (b) {
    this.set({"showHeader": b, "showPDFDownload": b});
  },
  setShowHeaderAndPdfDownload: function (b) {
    this.set({"showHeader": b, "showPDFDownload": b});
  },
  showRejectOption: function () {
    return this.get("showRejectOption");
  },
  setShowRejectOption: function (b) {
    this.set("showRejectOption", b);
  },
  allowRejectReason: function () {
    return this.get("allowRejectReason");
  },
  setAllowRejectReason: function (b) {
    this.set("allowRejectReason", b);
  },
  setShowRejectOptionAndAllowRejectReason: function (b) {
    this.set({"allowRejectReason": b, "showRejectOption": b});
  },
  showPDFDownload: function () {
    return this.get("showPDFDownload");
  },
  setShowPdfDownload: function (b) {
    this.set("showPDFDownload", b);
  },
  showFooter: function () {
    return this.get("showFooter");
  },
  setShowFooter: function (b) {
    this.set("showFooter", b);
  },
  ready: function () {
    return this.document().ready() && this.theme().ready();
  }
});

var SignviewSettingsView = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels: function () {
      return [this.props.model];
    },
    render: function () {
      var self = this;
      var model = self.props.model;
      if (!model.ready()) {
        return (<div/>);
      } else {
        return (
          <div className="signviewsettings">
            <div className="options">
              <Checkbox
                checked={model.showHeader()}
                label={localization.designview.signviewsettings.showheader}
                onChange={function (c) { model.setShowHeaderAndPdfDownload(c); }}
              />
              <div className="indented">
                <Checkbox
                  checked={model.showPDFDownload()}
                  label={localization.designview.signviewsettings.showpdfdownload}
                  onChange={function (c) {
                    if (c) {
                      model.setShowHeaderAndPdfDownload(c);
                    } else {
                      model.setShowPdfDownload(c);
                    }
                  }}
                />
              </div>
              <Checkbox
                checked={model.showRejectOption()}
                label={localization.designview.signviewsettings.showrejectoption}
                onChange={function (c) {
                  if (!c) {
                    model.setShowRejectOptionAndAllowRejectReason(c);
                  } else {
                    model.setShowRejectOptionAndAllowRejectReason(c);
                  }
                }}
              />
              <div className="indented">
                <Checkbox
                  checked={model.allowRejectReason()}
                  label={localization.designview.signviewsettings.allowrejectreason}
                  onChange={function (c) {
                    if (c) {
                      model.setShowRejectOptionAndAllowRejectReason(c);
                    } else {
                      model.setAllowRejectReason(c);
                    }
                  }}
                />
              </div>
              <Checkbox
                checked={model.showFooter()}
                label={localization.designview.signviewsettings.showfooter}
                onChange={function (c) { model.setShowFooter(c); }}
              />
            </div>
            <div className="container">
              <SignviewPreview
                showHeader={model.showHeader()}
                showRejectOption={model.showRejectOption()}
                allowRejectReason={model.allowRejectReason()}
                showPDFDownload={model.showPDFDownload()}
                showFooter={model.showFooter()}
                model={model.theme()}
              />
            </div>
          </div>
        );
      }
    }
});

module.exports = React.createClass({
  close: function () {
    this._model = null;
    this.props.onClose();
  },
  onAccept: function () {
    this.props.document.setShowheader(this._model.showHeader());
    this.props.document.setShowrejectoption(this._model.showRejectOption());
    this.props.document.setAllowrejectreason(this._model.allowRejectReason());
    this.props.document.setShowpdfdownload(this._model.showPDFDownload());
    this.props.document.setShowfooter(this._model.showFooter());
    this.props.document.save();

    this.close();
  },
  render: function () {
    if (!this._model) {
      this._model = new SignviewSettingsModel({document: this.props.document});
    }

    return (
      <Modal.Container width={940} active={this.props.active}>
        <Modal.Header
          title={localization.designview.signviewsettings.title}
          onClose={this.close}
          showClose={true}
        />
        <Modal.Content>
          <SignviewSettingsView model={this._model} />
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.close} />
          <Modal.AcceptButton
            text={localization.save}
            onClick={this.onAccept}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});
