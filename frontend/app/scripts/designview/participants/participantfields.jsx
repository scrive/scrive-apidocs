/** @jsx React.DOM */

define(['legacy_code', 'React', 'common/button', 'designview/participants/participantnamefield', 'designview/participants/participantfield', 'designview/participants/participantselectfield', 'designview/participants/participantnotnamedfield','designview/participants/participantaddfield'], function(_Legacy, React, Button, ParticipantNameField, ParticipantField, ParticipantSelectField, ParticipantNotNamedField, ParticipantAddField) {

return React.createClass({

  render: function() {
    var self = this;
    var sig = this.props.model;
    var viewmodel = this.props.viewmodel;

    return (
      <div className="design-view-action-participant-details-information">
        <ParticipantNameField model={sig}/>
        {/* if */ sig.emailField() != undefined &&
          <ParticipantField model={sig.emailField()}/>
        }
        {
          _.map(sig.fields(), function(f) {
            if (f.isBlank()) {
              return (<ParticipantSelectField model={f}/>)
            } else if (f.noName()) {
              return (<ParticipantNotNamedField model={f}/>)
            } else if (!f.isEmail() && !f.isFstName() && !f.isSndName() && f.isText()) {
              return (<ParticipantField model={f}/>)
            } else {
              return;
            }
          })
        }
        <ParticipantAddField model={sig}/>
        {/* if */ sig.isCsv() &&
          <div className="design-view-action-participant-details-information-field-wrapper">
            <Button
              text={localization.designview.viewCSV}
              type="optional"
              onClick={function() {
                mixpanel.track('Open CSV Popup');
                new CsvSignatoryDesignPopup({
                  designview: viewmodel
                });
              }}
            />
          </div>
       }
      </div>
    );
  }
});

});


/*
   var DesignViewParticipantDetailsView = Backbone.View.extend({
        className: 'design-view-action-participant-details',
        initialize: function(args) {
            var view = this;
            _.bindAll(this, 'render');
            view.viewmodel = args.viewmodel;
            view.participation = new DesignViewParticipation({model:view.model});
            view.newFieldSelector = new DesignViewNewFieldSelector({model:view.model});
            view.model.bind('change:fields', view.render);
            view.model.bind('change:authentication', view.render);
            view.model.bind('change:delivery', view.render);
            view.model.bind('change:confirmationdelivery', view.render);
            view.model.bind('change:csv', view.render);
            view.render();
        },
        destroy : function() {
          this.model.unbind('change:fields', this.render);
          this.model.unbind('change:authentication', this.render);
          this.model.unbind('change:delivery', this.render);
          this.model.unbind('change:confirmationdelivery', this.render);
          this.model.unbind('change:csv', this.render);
          this.off();
          if (this.participation != undefined) this.participation.destroy();
          this.remove();
        },
        render: function() {
            var view = this;
            var sig = view.model;

            if(!sig.isRemoved) {
                var div = $('<div />');

                div.append(view.detailsInformation());
                div.append(view.participation.el);

                view.$el.html(div.children());
            }
            return view;
        },
        detailsInformation: function() {
            var view = this;
            var sig = view.model;

            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information');
            div.append(view.detailsInformationFields());

            return div;
        },
        detailsInformationFields: function() {
            var view = this;
            var sig = view.model;

            var div = $("<div class='design-view-action-participant-details-information-fields'/>")
                        .append(view.detailsFullNameField())
                        .append(view.detailsInformationField('email', 'standard', localization.email));

            var ignores = ['fstname', 'sndname', 'email'];
            $.each(sig.fields(), function(i, e) {
                    if(e.isBlank())
                        div.append(view.detailsInformationNewField(e));
                    else if(e.noName())
                        div.append(view.detailsInformationCustomFieldName(e));
                    else if(!_.contains(ignores, e.name()) && e.isText())
                        div.append(view.detailsInformationField(e.name(), e.type(), e.nicename()));
            });

            if(sig.isCsv()) {
                var csvButton = new Button({
                    type: 'optional',
                    text: localization.designview.viewCSV,
                    onClick: function() {
                        mixpanel.track('Open CSV Popup');
                        new CsvSignatoryDesignPopup({
                            designview: view.viewmodel
                        });
                    }
                });
                var wrapperdiv = $("<div class='design-view-action-participant-details-information-field-wrapper'/>");
                wrapperdiv.append(csvButton.el());
                div.append(wrapperdiv);
            }
            div.append(view.newFieldSelector.el);

            return div;
        },
        detailsInformationCustomFieldName: function(field) {
            var self = this;
            var sig = self.model;
            var viewmodel = self.viewmodel;
            var input;

            var div = $("<div class='design-view-action-participant-details-information-field-wrapper'/>");

            var setter = function() {
                var value = input.value();
                console.log("Starting setter with value " + value);
                if(value) {
                    var samenameexists = _.any(sig.customFields(), function(c) { return value == c.name() && c != field;});
                    if (samenameexists) {
                      $(input.el()).addClass("conflict");
                      new FlashMessage({color: "error", content : localization.designview.fieldWithSameNameExists});
                      return;
                    }

                    mixpanel.track('Enter custom field name', {
                        'Field name': input.value()
                    });

                    field.setName(input.value());
                    sig.trigger('change:fields');
                    field.unbind('change', changer);
                }
            };
            var remover = function() {
                mixpanel.track('Click remove field', {
                    Type: field.type(),
                    Name: field.name()
                });
                field.removeAllPlacements();
                sig.deleteField(field);
            };

            var changer = function() {
                input.setValue(field.name());
            };

            input = new InfoTextInput({
                cssClass: 'design-view-action-participant-new-field-name-input',
                infotext: localization.designview.fieldName,
                value: '',
                onChange : function() { $(input.el()).removeClass("conflict");},
                onEnter: setter,
                onRemove : remover
            });

            field.bind('change', changer);

            if(!field.isValid(true))
                $(input.el()).addClass('redborder');
            else
                $(input.el()).removeClass('redborder');

            var button = new Button({
                text: localization.ok,
                width: 64,
                onClick: setter
            });


            div.append(input.el());
            div.append(button.el());

            return div;

        },
        detailsInformationNewField: function(field) {
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;

            var div = $("<div class='design-view-action-participant-details-information-field-wrapper'/>");

            var allFieldOptions = view.possibleFields.concat([]);

            function isUnique(field) {
                return _.every(allFieldOptions, function(o) {
                    return field.name() !== o.name && field.type() !== o.type;
                });
            }

            _.each(viewmodel.document().signatories(), function(signatory) {
                _.each(signatory.fields(), function(field) {
                    if(field.isText() && isUnique(field))
                        allFieldOptions.push({name: field.name(),
                                              type: field.type()});
                });
            });

            var options = [];

            // keep only fields not already part of signatory
            _.each(allFieldOptions, function(f) {
                if(!sig.field(f.name, f.type))
                    options.push({
                        name: view.placeholder(f.name),
                        value: f
                    });
            });

            options.push({name: localization.designview.customField,
                          value: {name: '--custom',
                                  type: '--custom'}}); // type is not used for custom

            var name;

            if(!view.selected)
                name = localization.designview.whatField;
            else if(view.selected.name === '--custom')
                name = localization.designview.customField;
            else
                name = view.placeholder(view.selected.name);
            var select = $("<div/>");
            React.render(React.createElement(NewSelect.Select,{
                options: options,
                name: name,
                cssClass : 'design-view-action-participant-new-field-select',
                border : "",
                textWidth: 270,
                optionsWidth: "297px",
                onRemove : function() {
                  mixpanel.track('Click remove field', {
                    Type: field.type(),
                    Name: field.name()
                  });
                  field.removeAllPlacements();
                  sig.deleteField(field);
                },
                onSelect: function(v) {
                  if(v.name === '--custom') {
                    mixpanel.track('Select field type', {
                      Type: 'custom'
                    });
                    field.setType('custom');
                  } else {
                    field.setType(v.type);
                    field.setName(v.name);
                    mixpanel.track('Select field type', {
                      Type: v.type,
                      Name: v.name
                    });
                  }
                  sig.trigger('change:fields');
                  return true;
                }
              })
              , select[0]);
            div.append(select);
            return div;

        },
        detailsFullNameField: function() {
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;

            var value = sig.name();
            var div = $("<div class='design-view-action-participant-details-information-field-wrapper'/>");
            var fstnameField = sig.fstnameField();
            var sndnameField = sig.sndnameField();

            var csvfield = fstnameField.isCsvField();
            var csvname = localization.designview.fullName + "(" + localization.designview.fromCSV + ")";

            var input = new InfoTextInput({
                cssClass: 'design-view-action-participant-details-information-field s-input-fullname' + (csvfield || sig.author() ? " transparent" : "" ),
                infotext: csvfield ? csvname : localization.designview.fullName,
                readonly : csvfield || sig.author(),
                value: value,
                onChange: function(val) {
                    var str = val.trim();
                    var i = str.indexOf(' ');
                    var f, s;
                    if(i >= 0) {
                        f = str.slice(0,i).trim();
                        s = str.slice(i+1).trim();
                    } else {
                        f = str.trim();
                        s = '';
                    }
                    if (sndnameField != undefined) {
                      fstnameField.setValue(f, {origin: input}); // arguments for event handler
                      sndnameField.setValue(s, {origin: input}); // arguments for event handler
                    } else {
                      fstnameField.setValue(str, {origin: input}); // arguments for event handler
                    }
                }
            });

            var onNameFieldChange = function(obj, args) {
              // check both name fields to see if full name should be highlighted
              if (!fstnameField.isValid(true) || (sndnameField != undefined && !sndnameField.isValid(true))) {
                input.el().addClass('redborder');
              } else {
                input.el().removeClass('redborder');
              }
              if (args === undefined || args.origin !== input) {
                // the check above was needed, to know if the change event originated
                // from directly editing this input (in which case we can skip
                // setting the value), otherwise when the input contains "John S"
                // and we backspace the 'S', the space is auto-removed.
                input.setValue(sig.name());
              }
            };

            fstnameField.bind('change', onNameFieldChange);
            if (sndnameField != undefined) {
              sndnameField.bind('change', onNameFieldChange);
            }
            if(!fstnameField.isValid(true) || (sndnameField != undefined && !sndnameField.isValid(true)))
                    input.el().addClass('redborder');
            else
                    input.el().removeClass('redborder');

            var optionOptions = sig.author()?['sender']:['signatory', 'sender'];

            div.append(input.el());
            return div;
        },
        detailsInformationField: function(name, type, placeholder) {
            if (placeholder == localization.phone) {
              // such an ugly hack to only change this placeholder in one place
              placeholder = localization.phonePlaceholder;
            }
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;

            var field = sig.field(name, type);
            if(!field)
                return null;

            var value = field.value();
            var csvfield = field.isCsvField();
            var csvname = (placeholder || name) + " (" + localization.designview.fromCSV + ")";
            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-field-wrapper');

            var input = new InfoTextInput({
                cssClass: 'design-view-action-participant-details-information-field s-input-' + name + ' ' + (csvfield || field.isAuthorUnchangeableField() ? "transparent" : ""),
                infotext: csvfield ? csvname : (placeholder || name),
                readonly : csvfield || field.isAuthorUnchangeableField(),
                value:  value,
                onChange: function(val) {
                    if(typeof val === 'string')
                        field.setValue(val.trim());
                },
                onRemove: (!field.canBeRemoved() ?
                            undefined :
                            function() {
                                mixpanel.track('Click remove field', {
                                    Type: field.type(),
                                    Name: field.name()
                                });
                                field.removeAllPlacements();
                                sig.deleteField(field);
                            })
            });

            field.bind('change', function() {
                if(!field.isValid(true))
                    input.el().addClass('redborder');
                else
                    input.el().removeClass('redborder');
                // skip updating input value if it's the same as field value (modulo whitespace)
                // because when someone has typed "somethin g^H" the whitespace would get deleted
                // which is not really what we want
                if (input.value().trim() !== field.value()) {
                  input.setValue(field.value());
                }
            });

            var optionOptions = ['optional', 'signatory', 'sender'];

            if(sig.author())
                optionOptions = _.without(optionOptions, 'signatory');

            if(name === 'email')
                optionOptions = _.without(optionOptions, 'optional');

            if(name === 'email' && sig.needsEmail())
                optionOptions = ['sender'];

            if(name === 'mobile' && sig.needsMobile())
                optionOptions = ['sender'];

            if(name === 'sigpersnr' && sig.needsPersonalNumber())
                optionOptions = _.without(optionOptions, 'optional');

            if(!field.isValid(true))
                input.el().addClass('redborder');
            else
                input.el().removeClass('redborder');

            div.append(input.el());

            return div;
        },
        possibleFields: [
            {name: "fstname",
             type: 'standard'},
            {name: "sndname",
             type: 'standard'},
            {name: "email",
             type: 'standard'},
            {name: "sigco",
             type: 'standard'},
            {name: "sigpersnr",
             type: 'standard'},
            {name: "sigcompnr",
             type: 'standard'},
            {name: "mobile",
         type: 'standard'}
        ],
        fieldNames: {
            fstname: localization.fstname,
            sndname: localization.sndname,
            email: localization.email,
            sigcompnr: localization.companyNumber,
            sigpersnr: localization.personalNumber,
            sigco: localization.company,
            mobile: localization.phone
        },
        placeholder: function(name) {
            return this.fieldNames[name] || name;
        }
    });


 */