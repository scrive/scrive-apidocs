var React = require("react");
var Select = require("../../common/select");
var _ = require("underscore");

module.exports = React.createClass({
  basicFields: function () {
    return [
       {type: "name", order: 1},
       {type: "name", order: 2},
       {type: "email"},
       {type: "company"},
       {type: "personal_number"},
       {type: "company_number"},
       {type: "mobile"}
    ];
  },
  allPossibleFields: function () {
    var res = this.basicFields();
    var document = this.props.model.signatory().document();
    function isNotOnList(field) {
      return _.every(res, function (o) {
        return field.type() !== o.type && field.name() !== o.name ;
      });
    }
    _.each(document.signatories(), function (signatory) {
      _.each(signatory.fields(), function (field) {
        if (field.isCustom()) {
          res.push({name: field.name(), type: field.type()});
        }
      });
    });
    return res;
  },
  niceFieldName: function (type, order, name) {
    if (type == "name" && order == 1) {
      return localization.fstname;
    } else if (type == "name" && order == 2) {
      return localization.sndname;
    } else if (type == "email") {
      return localization.email;
    } else if (type == "company_number") {
      return localization.companyNumber;
    } else if (type == "personal_number") {
      return localization.personalNumber;
    } else if (type == "company") {
      return localization.company;
    } else if (type == "mobile") {
      return localization.phone;
    } else {
      return name;
    }
  },
  fieldOptions: function () {
    var self = this;
    var sig = self.props.model.signatory();
    var allPossibleFields = self.allPossibleFields();
    var options = [];
    _.each(allPossibleFields, function (f) {
      if (!sig.hasField(f.type, f.order, f.name)) {
        if (f.name === undefined) {
          f.name = "";
        }
        options.push({name: self.niceFieldName(f.type, f.order, f.name), value: f});
      }
    });
    options.push({
      name: localization.designview.customField,
      value: {name: "--text", type: "--text"} // type is not used for custom
    });
    return options;
  },
  render: function () {
    var self = this;
    var field = this.props.model;
    var sig = field.signatory();
    return (
      <div className="design-view-action-participant-details-information-field-wrapper">
        <Select
           ref="select"
           name={localization.designview.whatField}
           className={"design-view-action-participant-new-field-select"}
           border={""}
           width={297}
           options={self.fieldOptions()}
           onSelect={function (v) {
             if (v.name === "--text") {
               mixpanel.track("Select field type", {
                  Type: "text"
                });
               field.setType("text");
             } else {
               field.setType(v.type);
               field.setName(v.name);
               mixpanel.track("Select field type", {
                  Type: v.type,
                  Name: v.name
                });
             }
           }}
          onRemove={function () {
            mixpanel.track("Click remove field", {
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
