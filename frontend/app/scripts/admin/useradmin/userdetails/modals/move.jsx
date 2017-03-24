var React = require("react");

var InfoTextInput = require("../../../../common/infotextinput");
var Modal = require("../../../../common/modal");
var NumberValidation = require("../../../../../js/validation.js").NumberValidation;
var Submit = require("../../../../../js/submits.js").Submit;

var COMPANY_ID_VALIDATION = new NumberValidation();

var MoveModal = React.createClass({
  propTypes: {
    companyid: React.PropTypes.string.isRequired,
    active: React.PropTypes.bool.isRequired,
    onAccept: React.PropTypes.func.isRequired,
    onCancel: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      companyid: this.props.companyid,
      companyNameOrError: ""
    };
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (prevProps.active == true && prevState.companyid != this.state.companyid) {
      this.loadCompanyName();
    }
  },
  componentWillReceiveProps: function (nextProps) {
    if (nextProps.active == true) {
      this.setState(this.getInitialState());
    }
  },
  loadCompanyName: function () {
    if (COMPANY_ID_VALIDATION.validateData(this.state.companyid)) {
      var self = this;

      var submit = new Submit({
        url: "/adminonly/companyadmin/details/" + this.state.companyid,
        ajaxsuccess: function (resp) {
          self.setState({
            companyNameOrError: "Company with name: " + resp.companyname
          });
        },
        ajaxerror: function () {
          self.setState({
            companyNameOrError: "No company matches the given ID"
          });
        }
      });

      submit.sendAjax();
    } else {
      this.setState({
        companyNameOrError: "Company ID can only contain numbers"
      });
    }
  },
  onCompanyidChange: function (newCompanyid) {
    this.setState({companyid: newCompanyid});
  },
  onAcceptButtonClick: function () {
    this.props.onAccept(this.state.companyid);
  },
  render: function (argument) {
    return (
      <Modal.Container
        className="user-details-modal-move"
        active={this.props.active}
      >
        <Modal.Header
          title="Move user to different company"
          showClose={true}
          onClose={this.props.onCancel}
        />
        <Modal.Content>
          <label>
            Company ID:
            <InfoTextInput
              infotext="ID"
              value={this.state.companyid}
              onChange={this.onCompanyidChange}
            />
            <div className="company-name">
              {this.state.companyNameOrError}
            </div>
          </label>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onCancel} />
          <Modal.AcceptButton text="Move" onClick={this.onAcceptButtonClick} />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = MoveModal;
