var React = require("react");

var localiseQesError = function (error) {
  if (error.tag == "verimi_qes_error") {
    if (error.contents.tag == "incompatible_signing_method")
      return localization.designview.verimiQes.errorIncompatibleSigningMethod;
    else if (error.contents.tag == "non_author_field")
      return localization.designview.verimiQes.errorNonAuthorField;
    else if (error.contents.tag == "field_editable_by_signatory")
      return localization.designview.verimiQes.errorFieldEditableBySignatory;
    else if (error.contents.tag == "order_conflict")
      return localization.designview.verimiQes.errorOrderConflict;
    else if (error.contents.tag == "missing_attachment")
      return localization.designview.verimiQes.errorMissingAttachment;
    else {
      return null;
    }
  } else {
    return null;
  }
};

module.exports = React.createClass({
  render: function () {
    return (
      <ul className={"design-view-verimi-errors-list"}>
        {_.filter(_.map(this.props.errors,
               function (err) {
                 const text = localiseQesError(err);
                 return text ? <li>{text}</li> : null;
               }), function (li) { return li != null; })
        }
      </ul>
    );
  }
});
