var React = require("react");
var Modal = require("../common/modal");
var Button = require("../common/button");
var LocalStorage = require("../../js/storage.js").LocalStorage;

module.exports = React.createClass({
  propTypes: {
      onDismiss: React.PropTypes.func.isRequired,
      siglinkid: React.PropTypes.isRequired,
      email: React.PropTypes.string
    },

  doSignup: function () {
    LocalStorage.set("seenpostsignview", this.props.siglinkid.toString(), true);
    window.location = fromTemplate.postSignViewRedirectUrl + "?email=" + this.props.email;
  },

  doDismiss: function () {
    LocalStorage.set("seenpostsignview", this.props.siglinkid.toString(), true);
    this.props.onDismiss();
  },

  render: function () {
    return <div><Modal.Container active={true}>
        <Modal.Header
          title={localization.postSignview.title}
        />
        <Modal.Content>
          <div>
          <em>{localization.postSignview.infoReceiveDocument}</em>
          <hr/>
          {localization.postSignview.proposal}
          <ul>
            <li>{"\u26AB " + localization.postSignview.reason1}</li>
            <li>{"\u26AB " + localization.postSignview.reason2}</li>
            <li>{"\u26AB " + localization.postSignview.reason3}</li>
            </ul>
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.AcceptButton
                text={localization.postSignview.signupButton}
                onClick={this.doSignup}
              />
          <Modal.ExtraButtons>
                <Button
                  text={localization.postSignview.dismissButton}
                  onClick={this.doDismiss}
                />
          </Modal.ExtraButtons>
        </Modal.Footer>
      </Modal.Container></div>;
  }});
