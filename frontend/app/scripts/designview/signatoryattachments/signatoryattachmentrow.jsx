var classNames = require("classnames");
var React = require("react");
var _ = require("underscore");

var DesignSignatoryAttachmentModel = require("./designsignatoryattachment");
var Select = require("../../common/select");

var IS_REQUIRED_SELECT_OPTIONS = [
  {
    name: localization.signatoryAttachments.required,
    value: true
  },
  {
    name: localization.signatoryAttachments.optional,
    value: false
  }
];

var SignatoryAttachmentRow = React.createClass({
  propTypes: {
    attachment: React.PropTypes.instanceOf(DesignSignatoryAttachmentModel).isRequired,
    signatories: React.PropTypes.array.isRequired,
    onRemove: React.PropTypes.func.isRequired
  },
  componentWillMount: function () {
    this.signatoryOptions = this.generateSignatoryOptions();
  },
  componentDidMount: function () {
    this.props.attachment.on("change", this.onAttachmentChange);
  },
  componentWillReceiveProps: function () {
    this.signatoryOptions = this.generateSignatoryOptions();
  },
  componentWillUnmount: function () {
    this.props.attachment.off("change", this.onAttachmentChange);
  },
  nameFromSignatory: function (signatory) {
    var result = signatory.nameOrEmail();

    if (signatory.isCsv()) {
      result = localization.csv.title;
    }

    if (result == "") {
      result = signatory.nameInDocument();
    }

    return result;
  },
  generateSignatoryOptions: function () {
    var self = this;
    var signatoryOptions = [];

    _.each(this.props.signatories, function (signatory) {
      if (signatory.signs() && !signatory.author()) {
        signatoryOptions.push({
          name: self.nameFromSignatory(signatory),
          value: signatory
        });
      }
    });

    return signatoryOptions;
  },
  onNameChange: function (event) {
    this.props.attachment.set("name", event.target.value);
  },
  onDescriptionChange: function (event) {
    this.props.attachment.set("description", event.target.value);
  },
  onIsRequiredSelectChange: function (newIsRequired) {
    this.props.attachment.set("isRequired", newIsRequired);

    return true;
  },
  onSignatorySelectChange: function (newSignatory) {
    this.props.attachment.set("signatory", newSignatory);

    return true;
  },
  onRemoveIconClick: function () {
    this.props.onRemove(this.props.attachment);
  },
  onAttachmentChange: function () {
    this.forceUpdate();
  },
  render: function () {
    var self = this;

    var isRequired = this.props.attachment.get("isRequired");
    var paperClipIconClassName = classNames({
      "signatory-required-attachment-icon": isRequired,
      "signatory-optional-attachment-icon": !isRequired
    });

    var signatoryOptions = _.rest(this.signatoryOptions, 0);
    if (!this.props.attachment.get("signatory")) {
      signatoryOptions.unshift({
        name: "",
        value: null,
        disabled: true,
        selected: true
      });
    }

    return (
      <tr>
        <td className="editSignatoryAttachmentTDName">
          <input
            className="editSignatoryAttachmentName"
            type="text"
            value={this.props.attachment.get("name")}
            onChange={this.onNameChange}
          />
        </td>
        <td className="editSignatoryAttachmentTDDescription">
          <textarea
            className="editSignatoryAttachmentDescription"
            value={this.props.attachment.get("description")}
            onChange={this.onDescriptionChange}
          />
        </td>
        <td className="editSignatoryAttachmentTDIcon">
          <div className={paperClipIconClassName} />
        </td>
        <td className="editSignatoryAttachmentTDSelect">
          <Select
            ref="selectIsRequired"
            isOptionSelected={function (option) {
              return option.value == isRequired;
            }}
            options={IS_REQUIRED_SELECT_OPTIONS}
            width={110}
            onSelect={this.onIsRequiredSelectChange}
          />
        </td>
        <td className="editSignatoryAttachmentTDSelect">
          <Select
            ref="selectSignatory"
            isOptionSelected={function (option) {
              return option.value == self.props.attachment.get("signatory");
            }}
            options={signatoryOptions}
            width={150}
            onSelect={this.onSignatorySelectChange}
          />
        </td>
        <td className="editSignatoryAttachmentTDIcon">
          <div
            className="removeSignatoryAttachmentIcon"
            onClick={this.onRemoveIconClick}
          >
            X
          </div>
        </td>
      </tr>
    );
  }
});

module.exports = SignatoryAttachmentRow;
