define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin", "common/infotextinput"],
  function (legacy_code, _, Backbone, React, BackboneMixin, InfoTextInput) {

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      signview: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      askForName: React.PropTypes.bool.isRequired,
      askForEmail: React.PropTypes.bool.isRequired,
      askForSSN: React.PropTypes.bool.isRequired,
      askForPhone: React.PropTypes.bool.isRequired
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    render: function () {
      var sig = this.props.model;
      var signview = this.props.signview;

      var fstnameField = sig.fstnameField();
      var sndnameField = sig.sndnameField();
      var nameClass = React.addons.classSet({
        "obligatory-input": true,
        "valid": !signview.askForName()
      });

      var emailField = sig.emailField();
      var emailClass = React.addons.classSet({
        "obligatory-input": true,
        "valid": !signview.askForEmail()
      });

      var ssnField = sig.personalnumberField();
      var ssnClass = React.addons.classSet({
        "obligatory-input": true,
        "valid": !signview.askForSSN()
      });

      var phoneField = sig.mobileField();
      var phoneClass = React.addons.classSet({
        "obligatory-input": true,
        "valid": !signview.askForPhone()
      });

      return (
        <span>
          <h2 className="title">Om dig</h2>
          <div className="column spacing fillbox">
            {/* if */ this.props.askForName &&
              <InfoTextInput
                ref="name"
                className={nameClass}
                infotext={localization.personalName}
                value={sig.name()}
                onChange={function (value) {
                  var str = value.trim();
                  var i = str.indexOf(" ");
                  var f = "";
                  var s = "";
                  if (i >= 0) {
                    f = str.slice(0, i).trim();
                    s = str.slice(i + 1).trim();
                  } else {
                    f = str.trim();
                    s = "";
                  }
                  if (sndnameField != undefined) {
                    fstnameField.setValue(f);
                    sndnameField.setValue(s);
                  } else {
                    fstnameField.setValue(str);
                  }
                }}
              />
            }
            {/* if */ this.props.askForEmail &&
              <InfoTextInput
                ref="email"
                className={emailClass}
                infotext={localization.email}
                value={emailField.value()}
                onChange={function (value) {emailField.setValue(value);}}
              />
            }
            {/* if */ this.props.askForSSN &&
              <InfoTextInput
                ref="ssn"
                className={ssnClass}
                infotext={localization.personalNumber}
                value={ssnField.value()}
                onChange={function (value) {ssnField.setValue(value);}}
              />
            }
            {/* if */ this.props.askForPhone &&
              <InfoTextInput
                ref="phone"
                className={phoneClass}
                infotext={localization.phonePlaceholder}
                value={phoneField.value()}
                onChange={function (value) {phoneField.setValue(value);}}
              />
            }
          </div>
          <div className="clearfix" />
        </span>
      );
    }
  });
});
