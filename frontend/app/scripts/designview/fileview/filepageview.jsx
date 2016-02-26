var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var SignatureView = require("./signatureview");
var CheckboxView = require("./checkboxview");
var TextView = require("./textview");
var FilePage = require("../../../js/files.js").FilePage;
var $ = require("jquery");

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
      closeAllTypeSetters: React.PropTypes.func.isRequired
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
          if (v.closeTypeSetter && v.props.model !== placement) {
            v.closeTypeSetter();
          } else if (v.openTypeSetter && v.props.model === placement) {
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
          <img src={imageSrc} />
          {/* if */ imageComplete &&
            this.renderFields()
          }
        </div>
      );
    }
  });
