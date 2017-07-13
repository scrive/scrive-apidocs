var React = require("react");
var _ = require("underscore");

var Checkbox = require("../../icons/checkbox");
var CreateFieldsMixin = require("./createfieldsmixin");
var DraggableMixin = require("./draggablemixin");
var DrawingUtils = require("../../common/drawing_utils");
var Field = require("../../../js/fields.js").Field;
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var FieldPlacementGlobal = require(
  "../../../js/fieldplacementglobal.js"
).FieldPlacementGlobal;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var RadioButton = require("../../icons/radiobutton");
var RadioGroupControls = require("../fileview/radiogroupcontrols");

var CONTROL_SIZE = DrawingUtils.RelToAbs(
  FieldPlacementGlobal.defaultRadiobuttonWRel,
  FieldPlacementGlobal.designviewPageWidth
) + FieldPlacementGlobal.radioButtonWrapperPadding * 2;

var DummyRadioGroupView = React.createClass({
  render: function () {
    return (
      <div className="radiogroup dummy">
        <RadioGroupControls.BackgroundView
          height={CONTROL_SIZE * 3}
          visible={true}
          width={CONTROL_SIZE}
          x={FieldPlacementGlobal.radioGroupBorderWidth * -1}
          y={FieldPlacementGlobal.radioGroupBorderWidth * -1}
        />

        {_.map([0, 1, 2], function (i) {
          var style = {
            height: CONTROL_SIZE,
            left: 0,
            padding: FieldPlacementGlobal.radioButtonWrapperPadding,
            top: CONTROL_SIZE * i,
            width: CONTROL_SIZE
          };

          return (
            <div key={i} className="control" style={style}>
              <RadioButton
                active={true}
                selected={false}
                pageWidth={FieldPlacementGlobal.designviewPageWidth}
                wrel={FieldPlacementGlobal.defaultRadiobuttonWRel}
              />
            </div>
          );
        })}

        <RadioGroupControls.AddButton
          visible={true}
          width={CONTROL_SIZE}
          x={0}
          y={CONTROL_SIZE * 3}
          onClick={_.noop}
        />
      </div>
    );
  }
});

var DraggableRadioGroup = React.createClass({
  mixins: [CreateFieldsMixin, DraggableMixin],
  newRadioGroupExpectedSize: function () {
    return {
      height: CONTROL_SIZE * 4,
      width: CONTROL_SIZE
    };
  },
  radioGroupHelper: function () {
    var document = this.document();
    var signatory = document.signatoriesWhoSign()[0] || document.author();
    var helper = $("<div class='placedradiogroup'/>");

    React.render(
      React.createElement(DummyRadioGroupView, {}),
      helper[0]
    );

    var defaultSize = this.newRadioGroupExpectedSize();

    helper.addClass(FieldPlacementGlobal.signatoryCSSClass(signatory));
    helper.css("width", defaultSize.width + "px");
    helper.css("height", defaultSize.height + "px");

    $(".radiogroup", helper).addClass(
      FieldPlacementGlobal.signatoryCSSClass(signatory)
    );

    return helper;
  },
  disableDragAndClickIfNoneCanSign: function (el) {
    var self = this;

    el.mousedown(function () {
      if (self.document().signatoriesWhoSign().length > 0) {
        el.draggable("enable");
      } else {
        el.draggable("disable");

        new FlashMessage({
          type: "error",
          content: localization.designview.dndDisabled
        });

        return false;
      }
    });
  },
  newRadioGroupPlacement: function (args) {
    var document = this.document();
    var signatory = document.signatoriesWhoSign()[0] || document.author();

    var field = new Field({
      type: "radiogroup",
      signatory: signatory,
      name: document.newRadioGroupName()
    });
    signatory.addField(field);

    var wrapperPadding = FieldPlacementGlobal.radioButtonWrapperPadding;
    var xAbs0 = args.dropx + wrapperPadding;
    var yAbs0 = args.dropy + wrapperPadding;
    var wAbs = DrawingUtils.RelToAbs(
      FieldPlacementGlobal.defaultRadiobuttonWRel, args.pageWidth
    );
    var hAbs = wAbs;

    for (var i = 0; i < 3; i++) {
      field.addPlacement(new FieldPlacement({
        field: field,
        page: args.page,
        xrel: DrawingUtils.AbsToRel(xAbs0, args.pageWidth),
        yrel: DrawingUtils.AbsToRel(
          yAbs0 + (wrapperPadding * 2 + hAbs) * i, args.pageHeight
        ),
        wrel: FieldPlacementGlobal.defaultRadiobuttonWRel,
        hrel: 0,
        fsrel: 0
      }));

      field.addRadioButtonValue(field.newRadioButtonValue(), {silent: true});
    }

    return field.placements()[0];
  },
  newRadioGroupPlacementFromPosition: function (args) {
    return this.newRadioGroupPlacement({
      page: args.page,
      xrel: DrawingUtils.AbsToRel(args.x, args.pageWidth),
      yrel: DrawingUtils.AbsToRel(args.y, args.pageHeight),
      wrel: FieldPlacementGlobal.defaultRadiobuttonWRel,
      hrel: 0,
      fsrel: 0,
      dropx: args.x,
      dropy: args.y,
      pageWidth: args.pageWidth,
      pageHeight: args.pageHeight
    });
  },
  componentDidMount: function () {
    var self = this;
    var el = $(this.getDOMNode());

    self.disableDragAndClickIfNoneCanSign(el);

    self.initializeOnClickCreatePlacement({
      el: el,
      isEnabledFunc: function () {
        return self.document().signatoriesWhoSign().length > 0;
      },
      expectedSizeFunc: self.newRadioGroupExpectedSize,
      newPlacementFromPosition: self.newRadioGroupPlacementFromPosition,
      openTypeSetterFor: self.props.openTypeSetterFor
    });

    self.initializeDraggable({
      el: el,
      cursorAt: {top: 7, left: 7},
      verticalOffset: 0,
      xAxisOffset: 0,
      yAxisOffset: 0,
      dropXOffset: FieldPlacementGlobal.radioGroupPlacementDDLeftOffset,
      dropYOffset: FieldPlacementGlobal.radioGroupPlacementDDTopOffset,
      helper: self.radioGroupHelper,
      onDropOnPage: function (page, x, y, pageW, pageH) {
        var newRadioGroupPlacement = self.newRadioGroupPlacement({
          page: page,
          xrel: (x + FieldPlacementGlobal.radioGroupBorderWidth) / pageW,
          yrel: (y + FieldPlacementGlobal.radioGroupBorderWidth) / pageH,
          wrel: FieldPlacementGlobal.defaultCheckboxWRel,
          hrel: 0,
          fsrel: 0,
          dropx: x,
          dropy: y,
          pageWidth: pageW,
          pageHeight: pageH
        });
        self.props.openTypeSetterFor(newRadioGroupPlacement);
      }
    });
  },
  render: function () {
    return (
      <div className="design-view-action-document-draggable design-view-action-document-draggable-radiogroup">
        <div className="design-view-action-document-draggable-wrapper">
          <div className="design-view-action-document-draggable-inner-wrapper">
            <div className="design-view-action-document-draggable-icon-wrapper">
              <div className="design-view-action-document-draggable-icon" />
            </div>
            <div className="design-view-action-document-draggable-text">
              <span>{localization.designview.radiogroup}</span>
            </div>
          </div>
        </div>
      </div>
    );
  }
});

module.exports = DraggableRadioGroup;
