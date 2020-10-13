var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var InfoTextInput = require("../../common/infotextinput");
var TaskMixin = require("../navigation/task_mixin");
var ViewSize = require("../viewsize");
var Task = require("../navigation/task");
var $ = require("jquery");
var classNames = require("classnames");

  var DetailsList = React.createClass({
    render: function () {
      var isVertical = this.props.isVertical;
      var childs = [];
      React.Children.forEach(this.props.children, function (child) {
        if (child) {
          childs.push(child);
        }
      });

      var leftClass = classNames({
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

  module.exports = React.createClass({
    mixins: [TaskMixin],

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
        askForEmail: signview.askForEmail() || signview.hasExtraEmailInput(),
        askForSSN: signview.askForSSNIfNotEID(),
        askForPhone: signview.askForPhoneIfNotPin() || signview.hasExtraMobileInput()
      });
    },

    createTaskFromRef: function (ref, isComplete) {
      return new Task({
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
        this.refocus();
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

    refocus: function () {
      var ref = this.state.focus;
      if (ref !== "" && this.refs[ref]) {
        this.refs[ref].focus();
      }
    },

    handleBlur: function () {
      this.setState({focus: ""});
    },

    handleFocus: function (ref) {
      var self = this;
      return function () {
        self.setState({focus: ref});
      };
    },

    localizationStringForSsn: function () {
      var model = this.props.model;
      var sig = model.document().currentSignatory();
      switch (sig.authenticationToSign()) {
        case "dk_nemid_cpr":
          return localization.eID.idName.cpr;
        case "dk_nemid_cvr":
          return localization.eID.idName.cvr;
        default:
          return localization.personalNumber;
      }
    },

    localizationStringForSsnInfoText: function () {
      var model = this.props.model;
      var sig = model.document().currentSignatory();
      switch (sig.authenticationToSign()) {
        case "dk_nemid_cpr":
          return localization.eID.infoText.cpr;
        case "dk_nemid_cvr":
          return localization.eID.infoText.cvr;
        default:
          return localization.personalNumber;
      }
    },

    render: function () {
      var self = this;
      var sig = this.props.model;
      var signview = this.props.signview;
      var mediumView = ViewSize.isMedium();

      var fstnameField = sig.fstnameField();
      var sndnameField = sig.sndnameField();
      var nameClass = classNames({
        "signview-input": true,
        "obligatory-input": true,
        "valid": !signview.askForName()
      });

      var emailField = sig.emailField();
      var emailClass = classNames({
        "signview-input": true,
        "obligatory-input": true,
        "valid": !signview.askForEmail()
      });

      var ssnField = sig.personalnumberField();
      var ssnClass = classNames({
        "signview-input": true,
        "obligatory-input": true,
        "censor-screenshot": sig.hidePN(),
        "valid": !signview.askForSSNIfNotEID()
      });

      var phoneField = sig.mobileField();
      var phoneClass = classNames({
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
                    onFocus={this.handleFocus("name")}
                    onBlur={this.handleBlur}
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
                    onChange={function (value) { emailField.setValue(value); }}
                    onFocus={this.handleFocus("email")}
                    onBlur={this.handleBlur}
                  />
                </DetailsItem>
              }
              {/* if */ this.state.askForSSN &&
                <DetailsItem htmlFor="ssn" title={this.localizationStringForSsn()}>
                  <InfoTextInput
                    id="ssn"
                    infotext={this.localizationStringForSsnInfoText()}
                    ref="ssn"
                    tabIndex={3}
                    className={ssnClass}
                    value={ssnField.value()}
                    onChange={function (value) { ssnField.setValue(value); }}
                    onFocus={this.handleFocus("ssn")}
                    onBlur={this.handleBlur}
                    inputtype="numeric"
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
                    onChange={function (value) { phoneField.setValue(value); }}
                    onFocus={this.handleFocus("phone")}
                    onBlur={this.handleBlur}
                    inputtype="numeric"
                  />
                </DetailsItem>
              }
            </DetailsList>
          </div>
        </div>
      );
    }
  });
