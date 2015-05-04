/** @jsx React.DOM */

define(['legacy_code', 'React', 'common/select'], function(_Legacy, React, Select) {

return React.createClass({
  basicFields : function() {
    return [
       {name: "fstname", type: 'standard'},
       {name: "sndname", type: 'standard'},
       {name: "email",  type: 'standard'},
       {name: "sigco",  type: 'standard'},
       {name: "sigpersnr",type: 'standard'},
       {name: "sigcompnr",type: 'standard'},
       {name: "mobile",  type: 'standard'}
    ];
  },
  allPossibleFields: function() {
    var res = this.basicFields();
    var document = this.props.model.signatory().document();
    function isNotOnList(field) {
      return _.every(res, function(o) {
        return field.name() !== o.name && field.type() !== o.type;
      });
    }
    _.each(document.signatories(), function(signatory) {
      _.each(signatory.fields(), function(field) {
        if(field.isText() && isNotOnList(field)) {
           res.push({name: field.name(), type: field.type()});
        }
      });
    });
    return res;
  },
  niceFieldName: function(name) {
    if (name == "fstname") {
      return localization.fstname;
    } else if (name == "sndname") {
      return localization.sndname;
    } else if (name == "email") {
      return localization.email;
    } else if (name == "sigcompnr") {
      return localization.companyNumber;
    } else if (name == "sigpersnr") {
      return localization.personalNumber;
    } else if (name == "sigco") {
      return localization.company;
    } else if (name == "mobile") {
      return localization.phone;
    } else {
      return name;
    }
  },
  fieldOptions : function() {
    var self = this;
    var sig = self.props.model.signatory();
    var allPossibleFields = self.allPossibleFields();
    var options = [];
    _.each(allPossibleFields, function(f) {
      if(!sig.field(f.name, f.type)) {
         options.push({ name: self.niceFieldName(f.name), value: f });
      }
    });
    options.push({
      name: localization.designview.customField,
      value: { name: '--custom', type: '--custom'} // type is not used for custom
    });
    return options;
  },
  render: function() {
    var self = this;
    var field = this.props.model;
    var sig = field.signatory();
    var NewSelect = Select.Select;
    return (
      <div className="design-view-action-participant-details-information-field-wrapper">
        <NewSelect
           name={localization.designview.whatField}
           cssClass={"design-view-action-participant-new-field-select"}
           border={""}
           textWidth={270}
           optionsWidth={"297px"}
           options={self.fieldOptions()}
           onSelect={function(v) {
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
          }}
          onRemove={function() {
              mixpanel.track('Click remove field', {
                Type: field.type(),
                Name: field.name()
              });
              field.removeAllPlacements();
              sig.deleteField(field);
          }}
        />
      </div>
    );
  }
});

});
