var React = require("react");

var Modal = require("../../common/modal");

var ResultModal = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    result: React.PropTypes.string,
    time: React.PropTypes.string,
    onClose: React.PropTypes.func.isRequired
  },
  render: function () {
    var title = localization.verification.success;
    if (this.props.result == "error") {
      title = localization.verification.error;
    } else if (this.props.result == "failed") {
      title = localization.verification.failed;
    }

    return (
      <Modal.Container
        ref="container"
        active={this.props.active}
        onClose={this.props.onClose}
      >
        <Modal.Header
          ref="header"
          showClose={true}
          title={title}
          onClose={this.props.onClose}
        />
        <Modal.Content ref="content">
          <div className="verificationResultModal">
            <div className="float-left">
              {this.props.result == "success" &&
                <div className="icon verificationSuccessIcon" />
              }
              {this.props.result == "error" &&
                <div className="icon verificationErrorIcon" />
              }
              {this.props.result == "failed" &&
                <div className="icon verificationFailedIcon" />
              }
            </div>
            <div className="float-right">
              {this.props.result == "success" &&
                <div className="message">
                  {localization.verification.time} {this.props.time}
                </div>
              }
              {this.props.result == "error" &&
                <div className="message">
                  <br />{localization.verification.errorMessage}
                </div>
              }
              {this.props.result == "failed" &&
                <div className="message">
                  <br />{localization.verification.failedMessage}
                </div>
              }
            </div>
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton
            ref="cancelButton"
            onClick={this.props.onClose}
          />
          <Modal.AcceptButton
            ref="acceptButton"
            onClick={this.props.onClose}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = ResultModal;
