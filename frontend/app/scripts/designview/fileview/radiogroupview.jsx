var classNames = require("classnames");
var React = require("react");
var $ = require("jquery");
var _ = require("underscore");

var DrawingUtils = require("../../common/drawing_utils");
var RadioGroupControls = require("./radiogroupcontrols");
var DraggableMixin = require("../editdocument/draggablemixin");
var Field = require("../../../js/fields.js").Field;
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var FieldPlacementGlobal = require(
  "../../../js/fieldplacementglobal.js"
).FieldPlacementGlobal;
var HasTypeSetterMixin = require("./hastypesettermixin");
var RadioGroupTypesetterView = require(
  "../typesetters/radiogrouptypesetterview"
);

var RadioGroupView = React.createClass({
  propTypes: {
    closeAllTypeSetters: React.PropTypes.func.isRequired,
    hideCoordinateAxes: React.PropTypes.func.isRequired,
    model: React.PropTypes.instanceOf(Field).isRequired,
    moveCoordinateAxes: React.PropTypes.func.isRequired,
    pageWidth: React.PropTypes.number.isRequired,
    pageHeight: React.PropTypes.number.isRequired,
    showCoordinateAxes: React.PropTypes.func.isRequired
  },
  mixins: [React.addons.PureRenderMixin, HasTypeSetterMixin],
  componentWillMount: function () {
    this.backgroundViewFrame = DrawingUtils.MakeZeroRect();
    this.addButtonOrigin = DrawingUtils.MakeZeroPoint();
    this.radioButtonViews = [];
    this.layoutAfterUpdate = false;

    this.props.model.on("change", this.onModelChange);
  },
  componentDidMount: function () {
    this.setNeedsLayout();
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (this.layoutAfterUpdate) {
      this.layoutAfterUpdate = false;
      this.setNeedsLayout();
    } else {
      this.layoutTypesetter();
    }
  },
  componentWillUnmount: function () {
    this.props.model.off("change", this.onModelChange);
  },
  getPlacement: function () {
    return this.placementAtIndex(0);
  },
  getTypeSetterClass: function () {
    return RadioGroupTypesetterView.RadioGroupTypesetterView;
  },
  placementAtIndex: function (index) {
    return this.props.model.placements()[index];
  },
  origin: function () {
    var firstPlacement = this.placementAtIndex(0);

    return {
      x: DrawingUtils.RelToAbs(firstPlacement.xrel(), this.props.pageWidth),
      y: DrawingUtils.RelToAbs(firstPlacement.yrel(), this.props.pageHeight)
    };
  },
  makeBackgroundViewFrame: function () {
    var addButtonFrame = DrawingUtils.GetViewFrame(this.refs.addButton);

    var buttonsXMax = [];
    var buttonsXMin = [];
    var buttonsYMax = [];
    var buttonsYMin = [];

    for (var i = 0; i < this.radioButtonViews.length; i++) {
      var button = this.radioButtonViews[i];
      var buttonFrame = DrawingUtils.GetViewFrame(button);

      if (button && buttonFrame) {
        buttonsXMax.push(Math.ceil(DrawingUtils.GetRectXMax(buttonFrame)));
        buttonsXMin.push(Math.floor(DrawingUtils.GetRectXMin(buttonFrame)));
        buttonsYMax.push(Math.ceil(DrawingUtils.GetRectYMax(buttonFrame)));
        buttonsYMin.push(Math.floor(DrawingUtils.GetRectYMin(buttonFrame)));
      }
    }

    var xMax = Math.min(_.max(buttonsXMax), this.props.pageWidth);
    var xMin = Math.max(_.min(buttonsXMin), 0);
    var yMax = Math.min(
      _.max(buttonsYMax), this.props.pageHeight
    );
    var yMin = Math.max(_.min(buttonsYMin), 0);

    return DrawingUtils.MakeRect(
      xMin - FieldPlacementGlobal.radioGroupBorderWidth,
      yMin - FieldPlacementGlobal.radioGroupBorderWidth,
      xMax - xMin,
      yMax - yMin
    );
  },
  makeAddButtonOrigin: function (backgroundViewFrame) {
    var addButtonFrame = DrawingUtils.GetViewFrame(this.refs.addButton);
    var addButtonSize = addButtonFrame.size;
    var backgroundViewOrigin = backgroundViewFrame.origin;
    var backgroundViewSize = backgroundViewFrame.size;
    var borderWidth = FieldPlacementGlobal.radioGroupBorderWidth;

    var newAddButtonOrigin = DrawingUtils.MakePoint(
      (
        backgroundViewOrigin.x +
        (backgroundViewSize.width - addButtonSize.width) / 2.0 +
        borderWidth
      ),
      (
        backgroundViewOrigin.y + backgroundViewSize.height + borderWidth
      )
    );

    return newAddButtonOrigin;
  },
  layoutTypesetter: function () {
    if (this.state.typeSetterComp && this.state.typeSetterComp.isMounted()) {
      var $backgroundView = $(this.refs.backgroundView.getDOMNode());
      var backgroundViewOffset = $backgroundView.offset();
      var $typesetterView = $(this.state.typeSetterComp.getDOMNode());

      $typesetterView.css({
        left: (
          backgroundViewOffset.left + $backgroundView.outerWidth() +
          FieldPlacementGlobal.textTypeSetterArrowOffset
        ),
        top: backgroundViewOffset.top
      });

      if (!$typesetterView.hasClass("placed")) {
        $typesetterView.addClass("placed");
      }
    }
  },
  layoutSubviews: function () {
    this.backgroundViewFrame = this.makeBackgroundViewFrame();
    this.addButtonOrigin = this.makeAddButtonOrigin(this.backgroundViewFrame);

    $(this.refs.backgroundView.getDOMNode()).css({
      height: this.backgroundViewFrame.size.height,
      left: this.backgroundViewFrame.origin.x,
      top: this.backgroundViewFrame.origin.y,
      width: this.backgroundViewFrame.size.width
    });

    $(this.refs.addButton.getDOMNode()).css({
      left: this.addButtonOrigin.x,
      top: this.addButtonOrigin.y
    });
  },
  radioButtonRefCallback: function (component) {
    this.radioButtonViews.push(component);
  },
  setNeedsLayout: function () {
    this.layoutSubviews();
    this.layoutTypesetter();
  },
  onModelChange: function () {
    this.layoutAfterUpdate = true;
    this.forceUpdate();
  },
  onAddButtonClick: function (event) {
    event.stopPropagation();
    event.preventDefault();

    var addButtonFrame = DrawingUtils.GetViewFrame(this.refs.addButton);
    var origin = this.origin();
    var firstPlacement = this.placementAtIndex(0);

    this.props.model.addRadioButtonValue(
      this.props.model.newRadioButtonValue(), {silent: true}
    );

    var newRadioButtonYRel = DrawingUtils.AbsToRel(
      addButtonFrame.origin.y, this.props.pageHeight
    );

    if (addButtonFrame.origin.y >= this.props.pageHeight - addButtonFrame.size.height) {
      newRadioButtonYRel -= firstPlacement.wrel() * 2;
    }

    this.props.model.addPlacement(new FieldPlacement({
      field: this.props.model,
      page: firstPlacement.page(),
      xrel: DrawingUtils.AbsToRel(
        addButtonFrame.origin.x + FieldPlacementGlobal.radioButtonWrapperPadding,
        this.props.pageWidth
      ),
      yrel: newRadioButtonYRel,
      wrel: firstPlacement.wrel(),
      hrel: 0,
      fsrel: 0
    }));
  },
  onRadioButtonDrag: function () {
    this.setNeedsLayout();
  },
  onRadioButtonDragStart: function () {
    if (!this.hasTypeSetter()) {
      this.props.closeAllTypeSetters();
      this.openTypeSetter();
    }
  },
  onRadioButtonDropOnPage: function (placement, page, x, y, pageW, pageH) {
    if (page != placement.page()) {
      this.onRadioButtonDropOutside(placement);
    } else {
      placement.set({
        xrel: DrawingUtils.AbsToRel(x, pageW),
        yrel: DrawingUtils.AbsToRel(y, pageH)
      });
    }
  },
  onRadioButtonDropOutside: function (placement) {
    this.props.model.removePlacement(placement);
  },
  onClick: function (event) {
    this.toogleTypeSetterAndCloseOther();
  },
  render: function () {
    var self = this;
    var origin = this.origin();
    var firstPlacement = this.props.model.placements()[0];

    var style = {
      left: origin.x,
      top: origin.y
    };

    var className = classNames(
      "radiogroup",
      FieldPlacementGlobal.signatoryCSSClass(this.props.model.signatory()),
      {
        "has-typesetter": this.hasTypeSetter(),
        "needs-sender-action": this.props.model.needsSenderAction()
      }
    );

    var addButtonWidth = (
      DrawingUtils.RelToAbs(
        FieldPlacementGlobal.defaultRadiobuttonWRel, self.props.pageWidth
      ) +
      FieldPlacementGlobal.radioButtonWrapperPadding * 2
    );

    return (
      <div onClick={this.onClick} className={className}>
        <div className="placedfield js-radiogroup" style={style} />

        <RadioGroupControls.BackgroundView
          ref="backgroundView"
          height={this.backgroundViewFrame.size.height}
          visible={this.hasTypeSetter()}
          width={this.backgroundViewFrame.size.width}
          x={this.backgroundViewFrame.origin.x}
          y={this.backgroundViewFrame.origin.y}
        />

        {_.map(this.props.model.placements(), function (item, index) {
          return (
            <RadioGroupControls.RadioButtonView
              key={item.cid}
              ref={self.radioButtonRefCallback}
              pageHeight={self.props.pageHeight}
              pageWidth={self.props.pageWidth}
              placement={item}
              closeAllTypeSetters={self.props.closeAllTypeSetters}
              hideCoordinateAxes={self.props.hideCoordinateAxes}
              moveCoordinateAxes={self.props.moveCoordinateAxes}
              showCoordinateAxes={self.props.showCoordinateAxes}
              onDrag={self.onRadioButtonDrag}
              onDragStart={self.onRadioButtonDragStart}
              onDropOnPage={self.onRadioButtonDropOnPage}
              onDropOutside={self.onRadioButtonDropOutside}
            />
          );
        })}

        <RadioGroupControls.AddButton
          ref="addButton"
          visible={this.hasTypeSetter()}
          x={this.addButtonOrigin.x}
          y={this.addButtonOrigin.y}
          width={addButtonWidth}
          onClick={this.onAddButtonClick}
        />
      </div>
    );
  }
});

module.exports = RadioGroupView;
