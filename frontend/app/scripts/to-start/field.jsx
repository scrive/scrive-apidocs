/** @jsx React.DOM */

define(['legacy_code', 'React', 'common/backbone_mixin', 'common/infotextinput', 'common/checkbox'], function(_legacy, React, BackboneMixin, InfoTextInput, Checkbox) {

return React.createClass({
  propTypes: {
    field: React.PropTypes.object,
    signviewbranding: React.PropTypes.object // sign view branding
  },
  mixins: [BackboneMixin.BackboneMixin],
  getBackboneModels: function() {
    return [this.props.field];
  },
  changeHandler: function(value) {
    var field = this.props.field;
    var newvalue = value;

    if (field.isCheckbox()) {
      newvalue = value ? "checked" : "";
    }

    field.setValue(newvalue);
  },
  labelClickHandler: function() { 
    var field = this.props.field;
    var isCheckbox = field.isCheckbox();

    // Certain child elements (everything not backed by a DOM node that would usually be wrapped in 
    if (isCheckbox) {
      this.refs.checkbox.handleClick();
    }
  },
  render: function() {
    var field = this.props.field;
    var readonly = field.isAuthorUnchangeableField();
    var obligatory = field.needsSenderAction();
    var empty = !field.value();
    var isCheckbox = field.isCheckbox();
    var isTextField = field.isText();
    var isChecked = isCheckbox && field.value() == "checked";
    var isPhoneField = field.isMobile();
    var style = {};


    if (field.isSignature()) return (<span />);

    var c = this.changeHandler;

    var classes = React.addons.classSet({
      'field-input': isTextField,
      'needs-sender-action': obligatory,
      'optional-input': !obligatory,
      'filled-in': !empty,
      'checkbox-input': isCheckbox
    });

    return (
      <div className="field-wrapper">
        <label onClick={this.labelClickHandler}>
          <div className="field-name">{(field.isMobile() && localization.phonePlaceholder) || field.nicename()}</div>

          {/*if*/ isTextField && 
            <InfoTextInput value={field.value()} style={style} onChange={c} className={classes} readonly={readonly}/>
          }

          {/*if*/ isCheckbox &&
            <div className={classes}>
              <Checkbox ref="checkbox" initiallyChecked={isChecked} style={style} label="" onChange={c} />
            </div>
          }

        </label>
      </div>
    );
  }
});


});
