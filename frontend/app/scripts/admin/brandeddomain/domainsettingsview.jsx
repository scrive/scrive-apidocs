/** @jsx React.DOM */

define(["React","tinycolor","common/backbone_mixin","admin/brandeddomain/domainviewmodel","legacy_code","common/button","common/uploadimagebutton","common/select","common/infotextinput"], function(React, tinycolor, BackboneMixin, DomainViewModel,_Legacy, Button, UploadImageButton,NewSelect,InfoTextInput) {

var DomainImagePropertyEditor = React.createClass({
  render: function() {
    var self = this;
    return (
      <div className="domain-property-editor domain-image-property">
        <div className="domain-image-property-title">
          <strong>{self.props.title}:</strong>
        </div>
        <div className="domain-text-property-edit">
          <img src={this.props.getValue()} className="favicon-image"/>
          <UploadImageButton
                text="Upload image"
                width={160}
                size="tiny"
                onUpload={function(image) {
                  self.props.setValue(image);
                }}
          />
        </div>
      </div>
    );
  }
});

var DomainTextPropertyEditor = React.createClass({
  render: function() {
    var self = this;
    return (
      <div className="domain-property-editor domain-text-property">
        <div className="domain-text-property-title">
          <strong>{self.props.title}</strong> {self.props.description}
        </div>
        <div className="domain-text-property-edit">
          <InfoTextInput
            value={self.props.getValue()}
            infotext=""
            onChange={function(v) {self.props.setValue(v)}}
            maxLength={self.props.maxLength}
          />
        </div>
      </div>
    );
  }
});

var DomainColorPropertyEditor = React.createClass({
  getInitialState: function() {
    return {
      showColorPicker : false,
    };
  },
  hideColorPicker : function() {
    this.setState({showColorPicker : false});
  },
  componentDidMount : function() {
    var self = this;
    var colorPicker = $(this.refs.colorPicker.getDOMNode()).ColorPicker({
      flat: true,
      color: this.props.getValue(),
      onChange: function(hsb, hex, rgb) {
        if (self.state.showColorPicker) {
          self.props.setValue("#" + hex);
        }
      }
    });
    this.setState({colorPicker : colorPicker});
    colorPicker.hover(undefined,function() {
      self.setState({showColorPicker : false});
    });
  },
  componentDidUpdate: function() {
     var self = this;
     if (this.state.colorPicker && self.state.showColorPicker) {
       this.state.colorPicker.ColorPickerSetColor(self.props.getValue());
     }
  },
  render: function() {
    var self = this;
    return (
      <div className="domain-property-editor domain-color-property">
        <div className="domain-color-property-editor">
          <div>
            <strong className="domain-color-property-title">
              {self.props.title}
            </strong>
            <div className="domain-color-property-editor-color-picker-with-icons">
              { /*if*/ this.props.icons &&
                  _.map(this.props.icons,function(i) {
                    return (
                      <div className={"icon status " + i} key={Math.random()}  style={{backgroundColor: self.props.getValue() }}>
                      </div>
                    )
                  })


              }
              <div className="domain-color-property-editor-color-picker">
                <InfoTextInput
                  ref="colorInput"
                  value={this.props.getValue()}
                  onChange={function(v) {
                    if (tinycolor(v).ok)
                      self.props.setValue(v);
                    }
                  }
                  onBlur={
                    function() {
                      self.refs.colorInput.setValue(self.props.getValue());
                    }
                  }
                />
                <div
                  className="color-display"
                  style={{backgroundColor: this.props.getValue()}}
                  onClick={function() {
                    if (!self.state.showColorPicker) {
                      self.props.onColorPickerOpen();
                    }
                    self.setState({showColorPicker : !self.state.showColorPicker});
                  }}
                />
                <div ref="colorPicker" className={"color-picker " + (self.state.showColorPicker ? "" : "hidden")}/>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
});

var DomainPropertyTitle = React.createClass({
  render: function() {
    var self = this;
    return (
      <div className="domain-property-editor domain-property-title">
        <strong>
          {self.props.title}
         </strong>
          {self.props.description}
      </div>
    );
  }
});


return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    saveSettings : function() {
      this.props.model.save(function() {
        new FlashMessage({type : "success", content: "Saved"});
      });
    },
    hideAllColorPickers : function() {
      var self = this;
      self.refs.participant1.hideColorPicker();
      self.refs.participant2.hideColorPicker();
      self.refs.participant3.hideColorPicker();
      self.refs.participant4.hideColorPicker();
      self.refs.participant5.hideColorPicker();
      self.refs.participant6.hideColorPicker();
      self.refs.draftColor.hideColorPicker();
      self.refs.errorColor.hideColorPicker();
      self.refs.initiatedColor.hideColorPicker();
      self.refs.sentColor.hideColorPicker();
      self.refs.deliveredColor.hideColorPicker();
      self.refs.openedColor.hideColorPicker();
      self.refs.reviewedColor.hideColorPicker();
      self.refs.signedColor.hideColorPicker();
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
        <div className='domain-settings-edit'>
          <div className="domain-settings-edit-panel">
            <div className="domain-settings-edit-panel-title">
              {localization.branding.settingsTitle}
            </div>
            <div className='domain-settings-edit-panel-column left'>
              <DomainTextPropertyEditor
                title="Domain URL"
                description="Url that has to match give domain.Does not apply to main scrive domain"
                getValue={function() {return model.getUrl()}}
                setValue={function(v) {return model.setUrl(v)}}
              />
              <DomainTextPropertyEditor
                title="Browser title"
                description="Used in title in browsers"
                getValue={function() {return model.browserTitle()}}
                setValue={function(v) {return model.setBrowserTitle(v)}}
              />
              <DomainTextPropertyEditor
                title="SMS originator"
                description="The name that is displayed to the recipient when receiving an SMS. Maximum 11 alpha-numeric characters."
                getValue={function() {return model.smsOriginator()}}
                setValue={function(v) {return model.setSmsOriginator(v)}}
                maxLength={11}
              />
              <DomainTextPropertyEditor
                title="Email originator"
                description="The name that is displayed to the recipient when receiving emails."
                getValue={function() {return model.emailOriginator()}}
                setValue={function(v) {return model.setEmailOriginator(v)}}
              />
              <DomainTextPropertyEditor
                title="Contact email"
                description="In places where the user can contact you this is the email address that will be used."
                getValue={function() {return model.contactEmail()}}
                setValue={function(v) {return model.setContactEmail(v)}}
              />
              <DomainTextPropertyEditor
                title="No replay email"
                description="This email address will be used as the no-reply address. Setting this address may cause mail delivery issues."
                getValue={function() {return model.noreplyEmail()}}
                setValue={function(v) {return model.setNoreplyEmail(v)}}
              />
            </div>
            <div className='domain-settings-edit-panel-column right'>
              <DomainImagePropertyEditor
                title="Favicon"
                getValue={function() {return model.favicon()}}
                setValue={function(v) {return model.setFavicon(v)}}
              />
              <DomainPropertyTitle
                title="Participant colours"
                description="These colours will help while designing a document to create a visual connection between the party and the placed fields. The colours should be as different as possible to easily distinguish them from each other. If there are more than 6 parties the colors will be repeated."
              />
              <DomainColorPropertyEditor
                ref="participant1"
                title="Participant 1"
                getValue={function() {return model.participantColor1()}}
                setValue={function(v) {return model.setParticipantColor1(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}

              />
            <DomainColorPropertyEditor
                ref="participant2"
                title="Participant 2"
                getValue={function() {return model.participantColor2()}}
                setValue={function(v) {return model.setParticipantColor2(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}

              />
              <DomainColorPropertyEditor
                ref="participant3"
                title="Participant 3"
                getValue={function() {return model.participantColor3()}}
                setValue={function(v) {return model.setParticipantColor3(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorPropertyEditor
                ref="participant4"
                title="Participant 4"
                getValue={function() {return model.participantColor4()}}
                setValue={function(v) {return model.setParticipantColor4(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorPropertyEditor
                ref="participant5"
                title="Participant 5"
                getValue={function() {return model.participantColor5()}}
                setValue={function(v) {return model.setParticipantColor5(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorPropertyEditor
                ref="participant6"
                title="Participant 6"
                getValue={function() {return model.participantColor6()}}
                setValue={function(v) {return model.setParticipantColor6(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainPropertyTitle
                title="Status and event colours"
                description="This is the document status indicated with an icon and a colour. Here you can modify the colors."
              />

              <DomainColorPropertyEditor
                ref="draftColor"
                title="Draft, template"
                icons={["draft","template"]}
                getValue={function() {return model.draftColor()}}
                setValue={function(v) {return model.setDraftColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorPropertyEditor
                ref="errorColor"
                title="Error, withdrawn,cancelled and timed-out"
                icons={["problem","cancelled","rejected","timeouted"]}
                getValue={function() {return model.cancelledColor()}}
                setValue={function(v) {return model.setCancelledColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorPropertyEditor
                ref="initiatedColor"
                title="Initiated"
                icons={["initiated"]}
                getValue={function() {return model.initatedColor()}}
                setValue={function(v) {return model.setInitatedColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorPropertyEditor
                ref="sentColor"
                title="Sent"
                icons={["sent"]}
                getValue={function() {return model.sentColor()}}
                setValue={function(v) {return model.setSentColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
            <DomainColorPropertyEditor
                ref="deliveredColor"
                title="Delivered"
                icons={["delivered"]}
                getValue={function() {return model.deliveredColor()}}
                setValue={function(v) {return model.setDeliveredColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
            <DomainColorPropertyEditor
                ref="openedColor"
                title="Email opened, prolonged"
                icons={["opened","prolonged"]}
                getValue={function() {return model.openedColor()}}
                setValue={function(v) {return model.setOpenedColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorPropertyEditor
                ref="reviewedColor"
                title="Reviewed online"
                icons={["read"]}
                getValue={function() {return model.reviewedColor()}}
                setValue={function(v) {return model.setReviewedColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorPropertyEditor
                ref="signedColor"
                title="Signed, sealed"
                icons={["signed","sealed"]}
                getValue={function() {return model.signedColor()}}
                setValue={function(v) {return model.setSignedColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}

              />
            </div>
          </div>
          <div className='save-button-area'>
            <Button
              type="action"
              text={localization.branding.save}
              className="save"
              onClick={function() {self.saveSettings();}}
            />
          </div>
        </div>
      );
    }
  });
});