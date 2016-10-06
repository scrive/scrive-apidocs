var React = require("react");
var Backbone = require("backbone");
var $ = require("jquery");

var BackboneMixin = require("../common/backbone_mixin").BackboneMixin;
var BrowserInfo = require("../../js/utils/browserinfo.js").BrowserInfo;
var Button = require("../common/button");
var Calendar = require("../../js/calendar.js").Calendar;
var Document = require("../../js/documents.js").Document;
var InfoTextInput = require("../common/infotextinput");
var Modal = require("../common/modal");

var AuthorViewAutomaticRemindersModel = Backbone.Model.extend({
  defaults: {
    onAction: function () {}
  },
  initialize: function (args) {
    if (this.document().autoremindtime() != undefined) {
      this.set({
        newdaystoremind: this.document().autoremindtime().diffDays()
      });
    } else {
      this.set({
        newdaystoremind: Math.max(1, Math.floor(this.maxDaysLeftToSign() / 2))
      });
    }
  },
  triggerOnAction: function () {
    if (this.get("onAction")) {
      this.get("onAction")();
    }
  },
  authorview: function () {
    return this.get("authorview");
  },
  maxDaysLeftToSign: function () {
    return Math.max(1, this.document().timeouttime().diffDays());
  },
  newdaystoremind: function () {
    return this.get("newdaystoremind");
  },
  setNewdaystoremind: function (newdaystoremind) {
    var old = this.get("newdaystoremind");
    if (newdaystoremind == undefined || (1 <= newdaystoremind || newdaystoremind <= this.maxDaysLeftToSign())) {
      this.set({"newdaystoremind": newdaystoremind}, {silent: true});
    }

    this.trigger("change");
  },
  document: function () {
    return this.get("document");
  },
  setautoreminder: function (days, callback) {
    var self = this;
    this.document().setautoreminder(days).sendAjax(function () {
      if (callback != undefined) {
        callback();
      }

      self.triggerOnAction();
    });
  }
});

module.exports = React.createClass({
  mixins: [BackboneMixin],
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired,
    onAction: React.PropTypes.func
  },
  getBackboneModels: function () {
    return [this._model];
  },
  getInitialState: function () {
    return {
      showModal: false
    };
  },
  componentWillMount: function () {
    this._model = new AuthorViewAutomaticRemindersModel({
      document: this.props.document,
      onAction: this.props.onAction
    });
  },
  componentDidMount: function () {
    var self = this;
    this._calendar = new Calendar({
      on: $(this.refs.calendarButton.getDOMNode()),
      days: this._model.newdaystoremind(),
      maxValue: this._model.maxDaysLeftToSign(),
      change: function (days) {
        self._model.setNewdaystoremind(days);
      }
    });
  },
  componentDidUpdate: function () {
    this.refs.daysInput.setValue(this.daysInputValue());
    this._calendar.setDays(this._model.newdaystoremind());
  },
  handleOpenModal: function () {
    this.setState({showModal: true});
  },
  onModalClose: function () {
    this.setState({showModal: false});
  },
  onModalAccept: function () {
    if (this._model.newdaystoremind() == undefined) {
      this._model.setNewdaystoremind(1);
    }

    var self = this;
    this._model.setautoreminder(this._model.newdaystoremind(), function () {
      self.onModalClose();
    });
  },
  onRemoveReminderClick: function () {
    var self = this;
    this._model.setautoreminder(undefined, function () {
      self.onModalClose();
    });
  },
  onDaysInputChange: function (value) {
    var days = parseInt(value);
    if (isNaN(days)) {
      days = undefined;
    } else {
      days = Math.min(this.props.document.daystosign(), days);
      days = Math.max(1, days);
    }

    this._model.setNewdaystoremind(days);
  },
  daysInputValue: function () {
    var value = "";
    if (this._model && this._model.newdaystoremind()) {
      value = this._model.newdaystoremind() + "";
    }

    return value;
  },
  render: function () {
    var detailsText = "";
    var buttonText = localization.autoreminders.setDate;
    var modalTitle = localization.autoreminders.setAutoReminderTitle;
    var modalSubtitle = localization.autoreminders.changeAutoreminderDescription;
    var acceptButtonText = localization.autoreminders.setAutoreminderButton;

    if (this.props.document.autoremindtime()) {
      detailsText = localization.autoreminders.willBeSentOn + ": ";
      detailsText += this.props.document.autoremindtime().toYMDString();

      buttonText = localization.autoreminders.changeDate;

      modalTitle = localization.autoreminders.changeAutoReminderTitle;
      modalSubtitle = localization.autoreminders.changeAutoreminderDescription;

      acceptButtonText = localization.autoreminders.changeAutoreminderButton;
    }

    var modalButtonStyle = {};
    if (BrowserInfo.isSmallScreen()) {
      modalButtonStyle.marginTop = "-10px";
    }

    return (
      <div className="grey-box auto-reminder">
        <div className="titleinfo">
          <div className="name">{localization.autoreminders.automaticRemindersTitle}</div>
        </div>
        <div className="inner">
          <div className="details">
            <div>{detailsText}</div>
            <Button
              style={{marginTop: "10px"}}
              text={buttonText}
              size="small"
              onClick={this.handleOpenModal}
            />
          </div>
        </div>

        <Modal.Container active={this.state.showModal} width={420}>
          <Modal.Header
            title={modalTitle}
            showClose={true}
            onClose={this.onModalClose}
          />
          <Modal.Content>
            <div>
              <div className="modal-subtitle">{modalSubtitle}</div>
              <div className="autoreminder-modal-content">
                <div className="line-before-calendar">
                  <span>{localization.autoreminders.dueDateIn} </span>
                  <span>{this.props.document.timeouttime().diffDays()} </span>
                  <span>{localization.autoreminders.days} ({this.props.document.timeouttime().toYMDString()})</span>
                </div>
                <div className="text">{localization.autoreminders.daysToRemind}:</div>
                <InfoTextInput
                  ref="daysInput"
                  infotext="1"
                  value={this.daysInputValue()}
                  onChange={this.onDaysInputChange}
                />
                <div className="text">{localization.designview.days}</div>
                <div ref="calendarButton" className="calendarbutton"></div>
              </div>
            </div>
          </Modal.Content>
          <Modal.Footer>
            <Modal.CancelButton onClick={this.onModalClose} />
            <Modal.AcceptButton
              text={acceptButtonText}
              onClick={this.onModalAccept}
            />
            { /* if */ this.props.document.autoremindtime() &&
              <Modal.ExtraButtons marginRight={10}>
                <Button
                  text={localization.autoreminders.removeAutoreminderButton}
                  onClick={this.onRemoveReminderClick}
                />
              </Modal.ExtraButtons>
            }
          </Modal.Footer>
        </Modal.Container>
      </div>
    );
  }
});
