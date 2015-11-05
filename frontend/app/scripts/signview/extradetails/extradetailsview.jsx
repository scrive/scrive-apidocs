define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin", "common/infotextinput",
  "signview/tasks/task_mixin"],
  function (legacy_code, _, Backbone, React, BackboneMixin, InfoTextInput, TaskMixin) {

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin, TaskMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      signview: React.PropTypes.instanceOf(Backbone.Model).isRequired
    },

    getInitialState: function () {
      return {
        askForName: false,
        askForEmail: false,
        askForSSN: false,
        askForPhone: false
      };
    },

    componentWillMount: function () {
      var signview = this.props.signview;

      this.setState({
        askForName: signview.askForName(),
        askForEmail: signview.askForEmail(),
        askForSSN: signview.askForSSN(),
        askForPhone: signview.askForPhone()
      });
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    createTaskFromRef: function (ref, isComplete) {
      return new PageTask({
        type: "extra-details",
        label: localization.docsignview.textfield,
        onArrowClick: function () {
          ref.focus();
        },
        isComplete: isComplete,
        el: $(ref.getDOMNode())
      });
    },

    createTasks: function () {
      var signview = this.props.signview;
      var tasks = [];

      if (this.state.askForName) {
        tasks.push(this.createTaskFromRef(this.refs.name, function () {
          return !signview.askForName();
        }));
      }

      if (this.state.askForEmail) {
        tasks.push(this.createTaskFromRef(this.refs.email, function () {
          return !signview.askForEmail();
        }));
      }

      if (this.state.askForSSN) {
        tasks.push(this.createTaskFromRef(this.refs.ssn, function () {
          return !signview.askForSSN();
        }));
      }

      if (this.state.askForPhone) {
        tasks.push(this.createTaskFromRef(this.refs.phone, function () {
          return !signview.askForPhone();
        }));
      }

      return tasks;
    },

    render: function () {
      var self = this;
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
        <div className="section spacing extradetails">
          <h2 className="title">{localization.docsignview.filladitionfieldslabel}</h2>
          <div className="column spacing fillbox">
            {/* if */ this.state.askForName &&
              <InfoTextInput
                ref="name"
                className={nameClass}
                infotext={localization.personalName}
                value={self.nameInputValue && self.nameInputValue.trim() === sig.name() ?
                         self.nameInputValue : sig.name()}
                onChange={function (value) {
                  self.nameInputValue = value; // We don't store it in state, since this would trigger rerendering
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
            {/* if */ this.state.askForEmail &&
              <InfoTextInput
                ref="email"
                className={emailClass}
                infotext={localization.email}
                value={emailField.value()}
                onChange={function (value) {emailField.setValue(value);}}
              />
            }
            {/* if */ this.state.askForSSN &&
              <InfoTextInput
                ref="ssn"
                className={ssnClass}
                infotext={localization.personalNumber}
                value={ssnField.value()}
                onChange={function (value) {ssnField.setValue(value);}}
              />
            }
            {/* if */ this.state.askForPhone &&
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
        </div>
      );
    }
  });
});
