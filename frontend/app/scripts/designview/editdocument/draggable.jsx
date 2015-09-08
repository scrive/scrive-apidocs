define(["legacy_code", "React", "designview/fileview/draggablefield"], function (legacy_code, React, draggableField) {
  return React.createClass({
    propTypes: {
      className: React.PropTypes.string.isRequired,
      buttonText: React.PropTypes.string.isRequired,
      fontSize: React.PropTypes.number,
      onAdd: React.PropTypes.func,
      fieldFactory: React.PropTypes.func
    },

    componentDidMount: function () {
      var isDisabledCallback = function (field) {
        if (!field.isSignature()) { return true; }

        var doc = field.signatory().document();

        if (doc.signatoriesWhoSign().length > 0) {
          return true;
        }

        new FlashMessage({type: "error", content: localization.designview.dndDisabled});
        return false;
      };

      if (this.props.fieldFactory) {
        draggableField($(this.getDOMNode()), this.props.fieldFactory, undefined,
          undefined, true, this.props.fontSize, this.props.onAdd, isDisabledCallback);
      }
    },

    render: function () {
      var divClass = {
        "design-view-action-document-draggable": true
      };

      divClass[this.props.className] = true;

      return (
        <div className={React.addons.classSet(divClass)}>
          <div className="design-view-action-document-draggable-wrapper">
            <div className="design-view-action-document-draggable-inner-wrapper">
              <div className="design-view-action-document-draggable-icon-wrapper">
                <div className="design-view-action-document-draggable-icon" />
              </div>
              <div className="design-view-action-document-draggable-text">
                <span>{this.props.buttonText}</span>
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
});
