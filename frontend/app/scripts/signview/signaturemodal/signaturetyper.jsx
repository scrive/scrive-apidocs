/* Content of modal for typing signature */

define(["legacy_code", "Backbone", "React", "common/infotextinput", "common/button",
        "common/select", "common/backbone_mixin"],
        function (_legacy, Backbone, React, InfoTextInput, Button, Select, BackboneMixin) {

// Number used to generate bigger final images. Quality thing. Scale has to be small. IE8 has 32k limit.
var SIGNATURE_PICTURE_SCALE = 2;
var TYPING_CANVAS_WIDTH = 772;

var SignatureTyperModel = Backbone.Model.extend({
  defaults: {
    text: "",
    font: "TalkingToTheMoon"
  },
  initialize: function (args) {
    this.loadFromTMPValue();
  },
  text: function () {
    return this.get("text");
  },
  empty: function () {
    return this.text() === "";
  },
  setText: function (v) {
    this.set({text: v});
    this.saveToTMPValue();
  },
  font: function () {
    return this.get("font");
  },
  setFont: function (v) {
    this.set({font: v});
    this.saveToTMPValue();
  },
  height: function () {
    return this.get("height");
  },
  width: function () {
    return this.get("width");
  },
  field: function () {
    return this.get("field");
  },
  modal: function () {
    return this.get("modal");
  },
  loadFromTMPValue: function () {
    var tmp = this.field().valueTMP();
    if (tmp != undefined) {
      this.set({text: tmp.text, font: tmp.font});
    }
  },
  saveToTMPValue: function () {
    this.field().setValueTMP({text: this.text(), font: this.font()});
  },
  imageSrc: function () {
    return "/text_to_image?width=" + this.imageWidth() + "&height=" + this.imageHeight() +
      "&font=" + this.font() + "&text=" + encodeURIComponent(this.text());

  },
  imageBase64Url: function () {
    return "/text_to_image?base64=true&width=" + (SIGNATURE_PICTURE_SCALE * this.width()) +
      "&height=" + (SIGNATURE_PICTURE_SCALE * this.height()) + "&font=" + this.font() +
      "&text=" + encodeURIComponent(this.text());
  },
  imageHeight: function () {
    return Math.floor(TYPING_CANVAS_WIDTH * this.height() / this.width());
  },
  imageWidth: function () {
    return TYPING_CANVAS_WIDTH;
  },
  saveImage: function (callback) {
    var field = this.field();
    if (this.text() === "") {
      field.setValue("");
      if (typeof callback === "function") {
        callback();
      }
    } else {
      $.ajax(this.imageBase64Url(), {
        cache: false,
        success: function (resp) {
          field.setValue(resp);
          if (typeof callback === "function") {
            callback();
          }
        }
      });
    }
  }
});

return React.createClass({
  mixins: [BackboneMixin.BackboneMixin],

  getBackboneModels: function () {
    return [this.state.model];
  },

  propTypes: {
    field: React.PropTypes.object,
    width: React.PropTypes.number,
    height: React.PropTypes.number,
    acceptText: React.PropTypes.string,
    onClose: React.PropTypes.func,
    onAccept: React.PropTypes.func
  },

  getInitialState: function () {
    var model =  new SignatureTyperModel({
      field:  this.props.field,
      width:  this.props.width,
      height: this.props.height
    });
    return {
      model: model,
      imageSrc: model.imageSrc()
    };
  },

  componentDidUpdate: function () {
    this.refreshImg();
  },

  refreshImg: function () {
    var self = this;
    if (this.isMounted() && this.state.imageSrc != this.state.model.imageSrc()) {
      if ($(this.refs.img.getDOMNode())[0].complete) {
        this.setState({imageSrc: this.state.model.imageSrc()});
      } else {
        setTimeout(function () {self.refreshImg();}, 100);
      }
    }
  },

  fontStyle: function (fontName, width, height, bgPos) {
    var style = {display: "inline-block", height:height, width: width, backgroundPosition: bgPos};
    var text = "";
    if (fontName == "JenniferLynne") {
      text = localization.pad.font1;
    } else if (fontName == "TalkingToTheMoon") {
      text = localization.pad.font2;
    } else {
      text = localization.pad.font3;
    }
    style.backgroundImage = "url(/text_to_image?width=200&height=50&transparent=true&left=true&font=" +
      fontName + "&text=" + encodeURIComponent(text) + ")";
    return style;
  },

  render: function () {
    var self = this;
    var model = this.state.model;
    var text = localization.signviewDrawSignatureHere;

    var bodyWidth = $("body").width();
    var bodyHeight = $("body").height();
    var canvasHeight = Math.round(TYPING_CANVAS_WIDTH * model.height() / model.width());
    var footerHeight = 100;
    var headerHeight = 0;
    var contentHeight = canvasHeight + footerHeight;

    var left = (bodyWidth - TYPING_CANVAS_WIDTH) / 2;
    var top = (bodyHeight - contentHeight) / 2;

    var largestWidth = 1040;

    var contentStyle;
    if (bodyWidth < largestWidth) {
      contentStyle = {
        top: (bodyHeight - (canvasHeight + footerHeight + headerHeight)) + "px",
        left: left + "px"
      };
    } else {
      contentStyle = {
        top: top,
        left: left + "px"
      };
    }

    var backgroundStyle = {
      width: TYPING_CANVAS_WIDTH + "px",
      height: canvasHeight + "px"
    };

    return (
      <div style={contentStyle} className="content">
        <div style={backgroundStyle} ref="drawingArea" className="drawing-area">
          <div style={backgroundStyle} className="background">
            <div className="instruction">
              <hr />
            </div>
          </div>
          <img
            ref="img"
            width={model.imageWidth()}
            height={model.imageHeight()}
            style={{
              width:model.imageWidth(),
              height:model.imageHeight()
            }}
            src={this.state.imageSrc}
          />
        </div>
        <div className="footer">
          <Button
            className="transparent-button float-left"
            text={model.empty() ? localization.cancel : localization.pad.cleanImage}
            onClick={function () {
              if (model.empty()) {
                self.props.onClose();
              } else {
                model.setText("");
                model.saveImage();
              }
            }}
          />
          <div className="typer">
            <label htmlFor="type">{localization.pad.typeSignatureNameField}</label>
            <InfoTextInput
              id="type"
              value={model.text()}
              onChange={function (val) {
                model.setText(val);
              }}
            />
          </div>
          <Button
            type="action"
            text={self.props.acceptText}
            className={model.empty() ? "inactive" : ""}
            onClick={function () {
              if (!model.empty()) {
                model.saveImage(function () {
                  self.props.onAccept();
                });
              }
            }}
          />
          <div className="clearfix" />
        </div>
      </div>
    );
  }
});

});
