var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var SignatureView = require("./signatureview");
var CheckboxView = require("./checkboxview");
var TextView = require("./textview");
var RadioGroupView = require("./radiogroupview");
var FilePage = require("../../../js/files.js").FilePage;
var Modal = require("../../common/modal");
var $ = require("jquery");
var Field = require("../../../js/fields.js").Field;

var Cross = require("../../icons/cross.svg");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(FilePage).isRequired,
      imageSrc: React.PropTypes.string.isRequired,
      imageComplete: React.PropTypes.bool.isRequired,
      imageWidth: React.PropTypes.number.isRequired,
      imageHeight: React.PropTypes.number.isRequired,
      showCoordinateAxes: React.PropTypes.func.isRequired,
      hideCoordinateAxes: React.PropTypes.func.isRequired,
      moveCoordinateAxes: React.PropTypes.func.isRequired,
      closeAllTypeSetters: React.PropTypes.func.isRequired,
      removePageFunc: React.PropTypes.func
    },

    mixins: [BackboneMixin.BackboneMixin],

    // TODO: should be replaced with only `change` event later.
    componentWillMount: function () {
      this.props.model.on("change:dragables", this.handleChange);
    },

    componentDidMount: function () {
      this.initDroppable();
    },

    componentWillUnmount: function () {
      this.props.model.off("change:dragables", this.handleChange);
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    initDroppable: function () {
      var self = this;
      var page = this.props.model;
      var pageEl = $(this.getDOMNode());
      pageEl.droppable({
        drop: function (event, ui) {
          var helper = $(ui.helper);
          var top = helper.offset().top - pageEl.offset().top - 1;
          var left = helper.offset().left - pageEl.offset().left - 1;
          var height = pageEl.height();
          var width = pageEl.width();
          var onDrop = $(ui.draggable).draggable("option", "onDrop");
          onDrop(page, left, top, width, height);
          self.handleChange();
          return false;
        }
      });
    },
    openTypeSetterOnThisPageFor: function (placement) {
      if (this.isMounted()) {
        _.each(this.refs, function (v) {
          var viewPlacement = v.props.model;
          if (_.isFunction(v.getPlacement)) {
            viewPlacement = v.getPlacement();
          }

          if (v.closeTypeSetter && viewPlacement !== placement) {
            v.closeTypeSetter();
          } else if (v.openTypeSetter && viewPlacement === placement) {
            v.openTypeSetter();
          }
        });
      }
    },

    closeAllTypeSettersOnThisPage: function () {
      if (this.isMounted()) {
        _.each(this.refs, function (v) {
          if (v.closeTypeSetter) {
            v.closeTypeSetter();
          }
        });
      }
    },

    renderFields: function () {
      var self = this;
      var page = self.props.model;
      var file = page.file();
      var imageWidth = self.props.imageWidth;
      var imageHeight = self.props.imageHeight;
      var doc = file.document();
      var renderedRadioGroups = [];

      return _.map(doc.allPlacements(), function (placement, index) {
        if (placement.page() === page.number()) {
          var field = placement.field();
          var args = {
            key: placement.cid,
            ref: "placement-" + placement.cid,
            model: placement,
            pageWidth: imageWidth,
            pageHeight: imageHeight,
            showCoordinateAxes: self.props.showCoordinateAxes,
            hideCoordinateAxes: self.props.hideCoordinateAxes,
            moveCoordinateAxes: self.props.moveCoordinateAxes,
            closeAllTypeSetters: self.props.closeAllTypeSetters
          };

          if (field.isSignature()) {
            return <SignatureView {...args} />;
          } else if (field.isCheckbox()) {
            return <CheckboxView {...args} />;
          } if (field.isText()) {
            return <TextView {...args} />;
          } if (field.isRadioGroup()) {
            // Radio group fields contain multiple placements (one for every
            // radio button). We should render the radio group view once per
            // unique radio group.
            if (renderedRadioGroups.indexOf(field.cid) == -1) {
              renderedRadioGroups.push(field.cid);
              args.key = field.cid;
              args.model = field;
              return <RadioGroupView {...args} />;
            }
          } else {
            throw new Error("unknown field type");
          }
        }
      });
    },

    handleChange: function () {
      if (this.isMounted()) {
        this.forceUpdate();
      }
    },

    render: function () {
      var page = this.props.model;
      var file = page.file();
      var doc = file.document();
      var imageSrc = this.props.imageSrc;
      var imageComplete = this.props.imageComplete;

      return (
        <div id={"page" + page.number()} className="pagediv">
          {/* if */ this.props.removePageFunc &&
            <Cross
              className="remove-page"
              onClick={this.props.removePageFunc}
            />
          }
          <img src={imageSrc} />
          {/* if */ imageComplete &&
            this.renderFields()
          }
        </div>
      );
    }
  });
