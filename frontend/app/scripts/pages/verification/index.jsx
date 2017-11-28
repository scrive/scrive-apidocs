var React = require("react");

var LoadingDialog = require("../../../js/loading.js");
var ResultModal = require("./resultmodal");
var Submits = require("../../../js/submits.js");
var UploadButton = require("../../common/uploadbutton");

var VerificationView = React.createClass({
  getInitialState: function () {
    return {
      showResultModal: false,
      verificationResult: null,
      verificationTime: null
    };
  },
  onResultModalClose: function () {
    this.setState(this.getInitialState());
  },
  onSubmitError: function () {
    LoadingDialog.LoadingDialog.close();
  },
  onSubmitSend: function () {
    LoadingDialog.LoadingDialog.open();
  },
  onSubmitSuccess: function (response) {
    LoadingDialog.LoadingDialog.close();

    var newState = this.getInitialState();
    newState.showResultModal = true;

    if (response.success) {
      newState.verificationResult = "success";
      newState.verificationTime = response.time;
    } else if (response.error) {
      newState.verificationResult = "error";
    } else {
      newState.verificationResult = "failed";
    }

    this.setState(newState);
  },
  onUploadComplete: function (input) {
    var submit = new Submits.Submit({
      method: "POST",
      ajax: true,
      url: "/verify",
      onSend: this.onSubmitSend,
      ajaxerror: this.onSubmitError,
      ajaxsuccess: this.onSubmitSuccess
    });
    submit.addInputs(input);
    submit.send();
  },
  render: function () {
    return (
      <div>
        <UploadButton
          name="file"
          size="big"
          submitOnUpload={true}
          text={localization.uploadButton}
          type="action"
          width={380}
          onUploadComplete={this.onUploadComplete}
        />
        <ResultModal
          active={this.state.showResultModal}
          result={this.state.verificationResult}
          time={this.state.verificationTime}
          onClose={this.onResultModalClose}
        />
      </div>
    );
  }
});

module.exports = VerificationView;
