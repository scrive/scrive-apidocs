/** @jsx React.DOM */

define(["Underscore", "React", "common/button", "common/select",
        "common/editabletext", "legacy_code"],
  function (_, React, Button, Select, EditableText) {

  var SELECT_OPTIONS_WIDTH = 218;
  var SELECT_TEXT_WIDTH = 191;

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      onSave: React.PropTypes.func.isRequired
    },

    getInitialState: function () {
      return {
        edit: false,
        buttonSize: 0
      };
    },

    componentDidUpdate: function (prevProps, prevState) {
      if (this.refs.button && this.state.buttonSize === 0) {
        var width = $(this.refs.button.getDOMNode()).width();
        /* istanbul ignore if */
        if (width > 0) {
          this.setState({buttonSize: width});
        }
      }
    },

    getOrCreateField: function (sig, name, type) {
      var newField = sig.field(name, type);

      if (newField) {
        return newField;
      }

      newField = new Field({
        signatory: sig,
        type: type,
        name: name,
        obligatory: true,
        fresh: false,
        shouldbefilledbysender: sig.author()
      });

      newField.addedByMe = true;

      return newField;
    },

    typeSelected: function () {
      var model = this.props.model;
      var field = model.field();

      return field.nicename();
    },

    typeOptions: function () {
      var self = this;
      var model = self.props.model;
      var field = model.field();
      var sig = field.signatory();
      var doc = sig.document();

      var fieldNames = {
        fstname: localization.fstname,
        sndname: localization.sndname,
        email: localization.email,
        sigcompnr: localization.companyNumber,
        sigpersnr: localization.personalNumber,
        sigco: localization.company,
        mobile: localization.phone
      };

      var standardFields = [
        "fstname",
        "sndname",
        "email",
        "sigco",
        "sigpersnr",
        "sigcompnr",
        "mobile"
      ];

      var options = [];

      options.push({
        name: localization.designview.customField,
        onSelect: function () {
          var newField = new Field({
            signatory: sig,
            type: "custom",
            name: doc.newCustomName(),
            obligatory: true,
            shouldbefilledbysender: sig.author()
          });

          sig.addField(newField);
          newField.addedByMe = true;

          model.changeField(newField);
          self.edit();
        }
      });

      _.each(standardFields, function (name) {
        options.push({
          name: fieldNames[name],
          onSelect: function () {
            var newField = self.getOrCreateField(sig, name, "standard");
            model.changeField(newField);
          }
        });
      });

      _.each(sig.fields(), function (f) {
        if (f.isCustom() && f.name() !== "") {
          options.push({
            name: f.name(),
            onSelect: function () {
              var newField = self.getOrCreateField(sig, f.name(), "custom");
              model.changeField(newField);
            }
          });
        }
      });

      return _.filter(options, function (opt) {
        return opt.name !== self.typeSelected();
      });
    },

    edit: function () {
      this.setState({edit: true});
    },

    selector: function () {
      this.setState({edit: false});
    },

    render: function () {
      var model = this.props.model;
      var field = model.field();

      var buttonSize = field.isCustom() ? this.state.buttonSize : 0;
      var optionsWidth = SELECT_OPTIONS_WIDTH - buttonSize;
      var textWidth = SELECT_TEXT_WIDTH - buttonSize;

      return (
        <div className="subtitle">
          {localization.designview.selectField}
          <div className="fieldTypeSetter-subtitle-select fieldTypeSetter-field-select">
            {this.state.edit && <EditableText edit={true} text={field.name()} onSave={this.props.onSave} />}
            {!this.state.edit &&
              <span>
                <div className="fieldTypeSetter-field-select-container">
                  <Select.Select
                    name={this.typeSelected()}
                    options={this.typeOptions()}
                    optionsWidth={optionsWidth + "px"}
                    textWidth={textWidth}
                    cssClass={"typesetter-obligatory-option"}
                    style={{fontSize: "16px"}}
                  />
                </div>
                {field.isCustom() &&
                  <div ref="button" className="fieldTypeSetter-field-button-container">
                    <Button
                      size="tiny"
                      text={localization.designview.editField}
                      onClick={this.edit}
                    />
                  </div>
                }
              </span>
            }
          </div>
        </div>
      );
    }
  });
});
