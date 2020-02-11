var React = require("react");

var InfoTextInput = require("../../../../common/infotextinput");
var Modal = require("../../../../common/modal");
var NumberValidation = require("../../../../../js/validation.js").NumberValidation;
var Submit = require("../../../../../js/submits.js").Submit;

var COMPANY_ID_VALIDATION = new NumberValidation();

var MoveModal = React.createClass({
  propTypes: {
    companyId: React.PropTypes.string.isRequired,
    active: React.PropTypes.bool.isRequired,
    onAccept: React.PropTypes.func.isRequired,
    onCancel: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      companyId: this.props.companyId,
      companyNameOrError: ""
    };
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (prevProps.active == true && prevState.companyId != this.state.companyId) {
      this.loadCompanyName();
    }
  },
  componentWillReceiveProps: function (nextProps) {
    if (nextProps.active == true) {
      this.setState(this.getInitialState());
    }
  },
  loadCompanyName: function () {
    if (COMPANY_ID_VALIDATION.validateData(this.state.companyId)) {
      var self = this;

      var submit = new Submit({
        url: "/adminonly/companyadmin/details/" + this.state.companyId,
        ajaxsuccess: function (resp) {
          var entityName;
          if (resp.parentid !== null) {
            var entity = resp.parentgrouppath[resp.parentgrouppath.length - 1];
            entityName = entity.group_name;
          } else {
            entityName = resp.companyname;
          }
          self.setState({
            companyNameOrError: "Company with name: " + entityName
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
    this.setState({companyId: newCompanyid});
  },
  onAcceptButtonClick: function () {
    this.props.onAccept(this.state.companyId);
  },
  render: function (argument) {
    return (
      <Modal.Container
        className="company-details-modal-merge"
        active={this.props.active}
      >
        <Modal.Header
          title="Merge this company to different company"
          showClose={true}
          onClose={this.props.onCancel}
        />
        <Modal.Content>
          <label>
            Company ID:
            <InfoTextInput
              infotext="ID"
              value={this.state.companyId}
              onChange={this.onCompanyidChange}
            />
            <div className="company-name">
              {this.state.companyNameOrError}
            </div>
          </label>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onCancel} />
          <Modal.AcceptButton
            text="Merge"
            onClick={this.onAcceptButtonClick}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = MoveModal;
