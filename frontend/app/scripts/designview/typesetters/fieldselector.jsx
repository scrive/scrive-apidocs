var _ = require("underscore");
var React = require("react");
var Button = require("../../common/button");
var Select = require("../../common/select");
var EditableText = require("../../common/editabletext");
var Backbone = require("backbone");
var $ = require("jquery");
var Field = require("../../../js/fields.js").Field;

  var SELECT_TEXT_WIDTH = 218;

  module.exports = React.createClass({
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

    getOrCreateField: function (sig, type, order, name) {
      var newField = sig.field(name, type, order);

      if (newField) {
        return newField;
      }
      newField = new Field({
        signatory: sig,
        type: type,
        order: order,
        name: name,
        obligatory: true,
        shouldbefilledbysender: sig.author()
      });

      sig.addField(newField);
      return newField;
    },

    typeOptions: function () {
      var self = this;
      var model = self.props.model;
      var field = model.field();
      var sig = field.signatory();
      var doc = sig.document();

      var standardFields = [
        {type: "name", order: 1, text: localization.fstname},
        {type: "name", order: 2, text: localization.sndname},
        {type: "email",  text: localization.email},
        {type: "company",  text: localization.company},
        {type: "personal_number",  text: localization.personalNumber},
        {type: "company_number",  text: localization.companyNumber},
        {type: "mobile",  text: localization.phone}
      ];

      var options = [];

      options.push({
        name: localization.designview.customFieldLabel,
        onSelect: function () {
          var newField = new Field({
            signatory: sig,
            type: "text",
            value: "",
            name: doc.newCustomName(),
            obligatory: true,
            shouldbefilledbysender: sig.author()
          });

          sig.addField(newField);
          model.changeField(newField);
          self.edit();
        }
      });

      _.each(standardFields, function (f) {
        options.push({
          name: f.text,
          selected: f.type == field.type(),
          onSelect: function () {
            var newField = self.getOrCreateField(sig, f.type, f.order);
            model.changeField(newField);
          }
        });
      });

      _.each(sig.fields(), function (f) {
        if (f.isCustom() && f.name() !== "") {
          options.push({
            name: f.name(),
            selected: field.type() == "text" && field.name() == f.name(),
            onSelect: function () {
              var newField = self.getOrCreateField(sig, "text", undefined, f.name());
              model.changeField(newField);
            }
          });
        }
      });

      return options;
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
      var textWidth = SELECT_TEXT_WIDTH - buttonSize;

      return (
        <div className="subtitle">
          {localization.designview.selectField}
          <div className="fieldTypeSetter-subtitle-select fieldTypeSetter-field-select">
            {this.state.edit && <EditableText edit={true} text={field.name()} onSave={this.props.onSave} />}
            {!this.state.edit &&
              <span>
                <div className="fieldTypeSetter-field-select-container">
                  <Select
                    options={this.typeOptions()}
                    width={textWidth}
                    className={"typesetter-obligatory-option"}
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
