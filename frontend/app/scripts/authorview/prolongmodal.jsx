var React = require("react");
var $ = require("jquery");

var Calendar = require("../../js/calendar.js");
var InfoTextInput = require("../common/infotextinput");
var Modal = require("../common/modal");

var ProlongModal = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    onAccept: React.PropTypes.func.isRequired,
    onClose: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      acceptVisible: true,
      days: "1"
    };
  },
  componentDidMount: function () {
    this.calendar = new Calendar.Calendar({
      on: $(this.refs.calendarButton.getDOMNode()),
      days: this.state.days,
      change: this.onCalendarChange
    });
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (prevState.days != this.state.days) {
      this.calendar.setDays(parseInt(this.state.days));
      this.setState({acceptVisible: (!isNaN(parseInt(this.state.days, 10)))});
    }
  },
  onAcceptButtonClick: function () {
    this.calendar.close();
    this.props.onAccept(this.state.days);
  },
  onCloseButtonClick: function () {
    this.calendar.close();
    this.props.onClose();
  },
  onHide: function () {
    this.setState(this.getInitialState());
  },
  onCalendarChange: function (value) {
    this.setState({days: "" + value});
  },
  onDaysInputChange: function (value) {
    this.setState({days: value});
  },
  render: function () {
    return (
      <Modal.Container
        active={this.props.active}
        width={382}
        onHide={this.onHide}
      >
        <Modal.Header
          showClose={true}
          title={localization.prolongmodal.title}
          onClose={this.onCloseButtonClick}
        />
        <Modal.Content>
          <div className="prolong-modal-content">
            <div className="text">{localization.prolongmodal.text}</div>
            <InfoTextInput
              ref="daysInput"
              infotext="1"
              value={this.state.days}
              onChange={this.onDaysInputChange}
            />
            <div className="text">{localization.prolongmodal.days}</div>
            <div ref="calendarButton" className="calendarbutton"></div>
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.onCloseButtonClick} />
          {this.state.acceptVisible &&
            <Modal.AcceptButton
              ref="acceptModalButton"
              text={localization.prolongmodal.button}
              onClick={this.onAcceptButtonClick}
            />
          }
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = ProlongModal;
