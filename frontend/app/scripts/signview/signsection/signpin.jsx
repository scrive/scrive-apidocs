define(["legacy_code", "Underscore", "Backbone", "React", "common/button", "common/infotextinput",
  "signview/tasks/task_mixin"],
  function (legacy_code, _, Backbone, React, Button, InfoTextInput, TaskMixin) {

  return React.createClass({
    mixins: [TaskMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      askForPhone: React.PropTypes.bool.isRequired,
      canSign: React.PropTypes.bool.isRequired,
      field: React.PropTypes.instanceOf(Field).isRequired,
      onReject: React.PropTypes.func.isRequired,
      onNext: React.PropTypes.func.isRequired
    },

    createTasks: function () {
      var tasks = [this.createSignTask()];

      if (this.props.askForPhone) {
        tasks.push(this.createPhoneTask());
      }

      return tasks;
    },

    createSignTask: function () {
      var self = this;
      var model = self.props.model;

      return new PageTask({
        type: "sign",
        onArrowClick: function () {
          self.props.onSign();
        },
        isComplete: function () {
          return !model.document().currentSignatoryCanSign();
        },
        el:  $(self.refs.signButton.getDOMNode()),
        onActivate: function () {
          mixpanel.track("Begin signature task");
        },
        onDeactivate: function () {
          mixpanel.track("Finish signature task");
        }
      });
    },

    createPhoneTask: function () {
      var self = this;
      var ref = this.refs.phoneInput;
      var model = this.props.model;

      return new PageTask({
        type: "extra-details",
        onArrowClick: function () {
          ref.focus();
        },
        isComplete: function () {
          return !model.askForPhone();
        },
        el: $(ref.getDOMNode())
      });
    },

    render: function () {
      var model = this.props.model;
      var canHaveRejectButton = model.hasRejectOption();
      var field = this.props.field;
      var askForPhone = this.props.askForPhone;
      var canSign = this.props.canSign;
      var onNext = this.props.onNext;

      var inputClass = React.addons.classSet({
        "obligatory-input": true,
        "valid": !model.askForPhone()
      });

      var buttonClass = React.addons.classSet({
        "button-block": true,
        "inactive": !canSign
      });

      var divClass = React.addons.classSet({
        "col-xs-6": !BrowserInfo.isSmallScreen(),
        "col-xs-12": BrowserInfo.isSmallScreen(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <h1>{localization.docsignview.pinSigning.signWithSMSPin}</h1>
          {/* if */ askForPhone &&
            <dl>
              <dt><label htmlFor="phone">{localization.phonePlaceholder}</label></dt>
              <dd>
                <InfoTextInput
                  id="phone"
                  infotext={localization.phoneInfoText}
                  autocomplete={false}
                  ref="phoneInput"
                  className={inputClass}
                  value={field.value()}
                  onChange={function (value) { field.setValue(value); }}
                />
              </dd>
            </dl>
          }
          {/* else */ !askForPhone &&
            <p className="pin-phone">
              {localization.docsignview.pinSigning.pinWillBeSentTo} <b>{field.value()}</b>
            </p>
          }
          <Button
            ref="signButton"
            type="action"
            className={buttonClass}
            text={localization.next}
            onClick={function (e) { if (canSign) { onNext(e) } }}
          />
          {/* if */ canHaveRejectButton &&
            <Button
              ref="rejectButton"
              className="transparent-button button-block"
              text={localization.process.rejectbuttontext}
              onClick={this.props.onReject}
            />
          }
        </div>
      );
    }
  });
});
