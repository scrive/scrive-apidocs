/* Content of modal for typing signature */

define(["legacy_code", "Backbone", "React", "common/infotextinput", "common/button",
        "common/select", "common/backbone_mixin"],
        function (_legacy, Backbone, React, InfoTextInput, Button, Select, BackboneMixin) {

// Number used to generate bigger final images. Quality thing. Scale has to be small. IE8 has 32k limit.
var signaturePictureScale = 2;

var SignatureTyperModel = Backbone.Model.extend({
  defaults: {
    text: "",
    font: "JenniferLynne"
  },
  initialize: function (args) {
    this.loadFromTMPValue();
  },
  text: function () {
    return this.get("text");
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
    return "/text_to_image?base64=true&width=" + (signaturePictureScale * this.width()) +
      "&height=" + (signaturePictureScale * this.height()) + "&font=" + this.font() +
      "&text=" + encodeURIComponent(this.text());
  },
  imageHeight: function () {
    return Math.floor(820 * this.height() / this.width());
  },
  imageWidth: function () {
    return 820;
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
    return (
      <div>
        <div className="header" style={{textAlign:"left", margin: "25px 39px 0px 39px"}}>
          <div style={{fontSize:"28px", lineHeight: "32px"}}>
            {localization.pad.typeSignatureBoxHeader}
          </div>
          <div/>
          <a className="modal-close" onClick={function () {self.props.onClose()}}/>
          <div style={{margin:"13px 0px", height:"42px"}}>
            <InfoTextInput
              ref="textInput"
              infotext={localization.pad.typeSignatureNameField}
              className="float-left"
              style={{marginRight:"10px", border: "1px solid #7A94B8", width:"170px"}}
              value={model.text()}
              onChange={function (val) {
                model.setText(val);
                }
              }
            />
            <div style={{width:"200px", float:"left"}}>
              <Select
                name=""
                className="float-left"
                style={self.fontStyle(model.font(), "200px", "40px", "10px -3px")}
                width={200}
                options={[
                  {
                    name: "",
                    disabled: (model.font() == "JenniferLynne"),
                    style: self.fontStyle("JenniferLynne", "120px", "40px", "0px -15px"),
                    onSelect: function () {model.setFont("JenniferLynne");}
                  },
                  {
                    name: "",
                    disabled: (model.font() == "TalkingToTheMoon"),
                    style: self.fontStyle("TalkingToTheMoon", "120px", "40px", "0px -15px"),
                    onSelect: function () {model.setFont("TalkingToTheMoon");}
                  },
                  {
                    name: "",
                    disabled: (model.font() == "TheOnlyException"),
                    style: self.fontStyle("TheOnlyException", "120px", "40px", "0px -15px"),
                    onSelect: function () {model.setFont("TheOnlyException");}
                  }
                ]}
              />
            </div>
          </div>
        </div>
        <div className="signatureDrawingBoxWrapper" style={{width: "820px"}}>
          <div className="signatureDrawingBox">
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
        </div>
        <div>
          <div  className="modal-footer">
            <Button
              type="action"
              size="small"
              className="bottom-button accept-button"
              text={self.props.acceptText}
              onClick={function () {
                model.saveImage(function () {
                  self.props.onAccept();
                });
              }}
            />
            <label
              className="delete"
              style={{float:"left", marginRight:"20px", lineHeight: "40px"}}
              onClick={function () {
                self.props.onClose();
              }}
            >
              {localization.cancel}
            </label>
            <Button
              size="small"
              style={{float:"left", marginTop: "-2px;"}}
              text={localization.pad.cleanImage}
              onClick={function () {
                model.setText("");
                self.refs.textInput.focus();
              }}
            />
          </div>
        </div>
      </div>
    );
  }
});

});
