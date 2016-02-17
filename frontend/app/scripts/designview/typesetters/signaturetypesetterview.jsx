var React = require("react");
var TypeSetterMixin = require("./typesettermixin");
var SignatorySelector = require("./signatoryselector");
var Obligatory = require("./obligatory");
var More = require("./more");
var Anchor = require("./anchor");
var Done = require("./done");
var Remove = require("./remove");
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;

  module.exports = React.createClass({
    mixins: [TypeSetterMixin],

    verticalOffset: FieldPlacementGlobal.signatureTypeSetterVerticalOffset,

    horizontalOffset: FieldPlacementGlobal.signatureTypeSetterHorizontalOffset,

    renderBody: function () {
      var field = this.props.model.field();

      return (
        <span>
          <SignatorySelector field={field} />
          <Obligatory field={field} />
          <More>
            <Anchor model={this.props.model} />
          </More>
          <Done field={field} onDone={this.done} />
          <Remove model={this.props.model} />
        </span>
      );
    }
  });
