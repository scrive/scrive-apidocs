var classNames = require("classnames");
var React = require("react");
var $ = require("jquery");

var Button = require("../common/button");
var FlashMessages = require("../../js/flashmessages");
var HtmlTextWithSubstitution = require("../common/htmltextwithsubstitution");
var Submits = require("../../js/submits.js");

var AcceptTOSView = React.createClass({
  getInitialState: function () {
    return {
      accepted: false,
      hasError: false
    };
  },
  componentDidMount: function () {
    var link = $(".is-TOS", this.refs.label.getDOMNode());
    link.attr("class", "clickable");
    link.attr("target", "_blank");
    link.attr("href", "/terms-of-service/");
    link.text(" " + link.text());
  },
  onAcceptButtonClick: function (event) {
    this.setState({hasError: !this.state.accepted});

    if (this.state.accepted) {
      var submit = new Submits.Submit({
        url: "/accepttos",
        method: "POST",
        ajax: true,
        tos: "on",
        ajaxsuccess: this.onSubmitSuccess
      });
      submit.send();
    }
  },
  onCheckboxClick: function (event) {
    this.setState({accepted: !this.state.accepted});
  },
  onSubmitSuccess: function (response) {
    new FlashMessages.FlashMessageAfterReload({
      content: localization.account.accountDetails.detailSaved,
      type: "success"
    });
    window.location.assign("/newdocument");
  },
  render: function () {
    var checkboxWrapperClassName = classNames("position", "first", {
      "has-error": this.state.hasError
    });

    var checkboxClassName = classNames("checkbox", {
      checked: this.state.accepted
    });

    return (
      <div className="short-input-container">
        <div className="short-input-container-body-wrapper">
          <div className="short-input-container-body">
            <div ref="wrapper" className={checkboxWrapperClassName}>
              <div
                ref="checkbox"
                className={checkboxClassName}
                onClick={this.onCheckboxClick}
              >
                <div className="checkmark" />
              </div>
              <HtmlTextWithSubstitution
                ref="label"
                secureText={localization.accountSetupModal.modalAccountSetupTOS}
                onClicks={{
                  "label": this.onCheckboxClick
                }}
              />
            </div>
            <div className="position">
              <Button
                ref="acceptButton"
                size="small"
                text={localization.accept}
                type="action"
                onClick={this.onAcceptButtonClick}
              />
            </div>
          </div>
        </div>
      </div>
    );
  }
});

module.exports = AcceptTOSView;
