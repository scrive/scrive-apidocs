define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin", "common/infotextinput",
  "signview/tasks/task_mixin", "signview/viewsize"],
  function (legacy_code, _, Backbone, React, BackboneMixin, InfoTextInput, TaskMixin, ViewSize) {

  var DetailsList = React.createClass({
    render: function () {
      var isVertical = this.props.isVertical;
      var childs = React.Children.map(this.props.children, function (child) {
        return child;
      });
      childs = _.compact(childs);

      var leftClass = React.addons.classSet({
        "col-md-6": true,
        "pull-right": !(childs[1] || childs[2])
      });

      return (
        <span>
          {/* if */ isVertical &&
          <div className="row">
            <dl className="col-xs-12">
              {childs[0]}
              {childs[1]}
              {childs[2]}
              {childs[3]}
            </dl>
          </div>
          }
          {/* else */ !isVertical &&
            <div className="row">
              {/* if */ (childs[0] || childs[3]) &&
                <dl className={leftClass}>
                  {childs[0]}
                  {childs[3]}
                </dl>
              }
              {/* if */ (childs[1] || childs[2]) &&
                <dl className="col-md-6">
                  {childs[1]}
                  {childs[2]}
                </dl>
              }
            </div>
          }
        </span>
      );
    }
  });

  var DetailsItem = React.createClass({
    render: function () {
      return (
        <div className="item">
          <dt><label htmlFor={this.props.htmlFor}>{this.props.title}</label></dt>
          <dd>{this.props.children}</dd>
        </div>
      );
    }
  });

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin, TaskMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      signview: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      isVertical: React.PropTypes.bool.isRequired
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
        askForSSN: signview.askForSSNIfNotEID(),
        askForPhone: signview.askForPhoneIfNotPin()
      });
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    createTaskFromRef: function (ref, isComplete) {
      return new PageTask({
        type: "extra-details",
        onArrowClick: function () {
          ref.focus();
        },
        isComplete: isComplete,
        el: $(ref.getDOMNode())
      });
    },

    componentDidUpdate: function (prevProps) {
      if (this.props.isVertical !== prevProps.isVertical) {
        this.forceUpdateTasks();
      }
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
          return !signview.askForSSNIfNotEID();
        }));
      }

      if (this.state.askForPhone) {
        tasks.push(this.createTaskFromRef(this.refs.phone, function () {
          return !signview.askForPhoneIfNotPin();
        }));
      }

      return tasks;
    },

    render: function () {
      var self = this;
      var sig = this.props.model;
      var signview = this.props.signview;
      var mediumView = ViewSize.isMedium();

      var fstnameField = sig.fstnameField();
      var sndnameField = sig.sndnameField();
      var nameClass = React.addons.classSet({
        "signview-input": true,
        "obligatory-input": true,
        "valid": !signview.askForName()
      });

      var emailField = sig.emailField();
      var emailClass = React.addons.classSet({
        "signview-input": true,
        "obligatory-input": true,
        "valid": !signview.askForEmail()
      });

      var ssnField = sig.personalnumberField();
      var ssnClass = React.addons.classSet({
        "signview-input": true,
        "obligatory-input": true,
        "valid": !signview.askForSSNIfNotEID()
      });

      var phoneField = sig.mobileField();
      var phoneClass = React.addons.classSet({
        "signview-input": true,
        "obligatory-input": true,
        "valid": !signview.askForPhoneIfNotPin()
      });

      return (
        <div className="section extradetails">
          <div className={mediumView ? "col-sm-6" : "col-sm-4"}>
            <h1 className="title">{localization.docsignview.filladitionfieldslabel}</h1>
          </div>
          <div className={mediumView ? "col-sm-6" : "col-sm-8"}>
            <DetailsList isVertical={this.props.isVertical}>
              {/* if */ this.state.askForName &&
                <DetailsItem htmlFor="name" title={localization.personalName}>
                  <InfoTextInput
                    id="name"
                    infotext={localization.nameInfoText}
                    ref="name"
                    tabIndex={1}
                    className={nameClass}
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
                </DetailsItem>
              }
              {/* if */ this.state.askForEmail &&
                <DetailsItem htmlFor="email" title={localization.email}>
                  <InfoTextInput
                    id="email"
                    infotext={localization.emailInfoText}
                    ref="email"
                    inputtype="email"
                    tabIndex={2}
                    className={emailClass}
                    value={emailField.value()}
                    onChange={function (value) {emailField.setValue(value);}}
                  />
                </DetailsItem>
              }
              {/* if */ this.state.askForSSN &&
                <DetailsItem htmlFor="ssn" title={localization.personalNumber}>
                  <InfoTextInput
                    id="ssn"
                    infotext={localization.ssnInfoText}
                    ref="ssn"
                    tabIndex={3}
                    className={ssnClass}
                    value={ssnField.value()}
                    onChange={function (value) {ssnField.setValue(value);}}
                  />
                </DetailsItem>
              }
              {/* if */ this.state.askForPhone &&
                <DetailsItem htmlFor="phone" title={localization.phonePlaceholder}>
                  <InfoTextInput
                    id="phone"
                    infotext={localization.phoneInfoText}
                    ref="phone"
                    inputtype="tel"
                    tabIndex={4}
                    className={phoneClass}
                    value={phoneField.value()}
                    onChange={function (value) {phoneField.setValue(value);}}
                  />
                </DetailsItem>
              }
            </DetailsList>
          </div>
        </div>
      );
    }
  });
});
