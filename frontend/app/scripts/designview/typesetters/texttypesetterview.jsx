/** @jsx React.DOM */

var imports = ["Underscore", "React", "common/select",
               "designview/typesetters/typesettermixin", "designview/typesetters/done",
               "legacy_code"];

define(imports, function (_, React, Select, TypeSetterMixin, Done) {

  return React.createClass({
    mixins: [TypeSetterMixin],

    verticalOffset: FieldPlacementGlobal.textTypeSetterVerticalOffset,

    horizontalOffset: FieldPlacementGlobal.textTypeSetterArrowOffset,

    obligatorySelected: function () {
      var model = this.props.model;
      var field = model.field();

      if (field.isOptional()) {
        return localization.designview.optionalField;
      }

      if (field.shouldbefilledbysender()) {
        return localization.designview.mandatoryForSender;
      }

      return localization.designview.mandatoryForRecipient;
    },

    obligatoryOptions: function () {
      var model = this.props.model;
      var field = model.field();

      var options = [];

      if (!field.shouldbefilledbysender()) {
        options.push({
          name: localization.designview.mandatoryForSender,
          onSelect: function () {
            field.makeObligatory();
            field.setShouldBeFilledBySender(true);
            field.authorObligatory = "sender";
            field.addedByMe = false;
          }
        });
      }

      if ((field.shouldbefilledbysender() || field.isOptional()) && field.canBeSetByRecipent()) {
        options.push({
          name: localization.designview.mandatoryForRecipient,
          onSelect: function () {
            field.makeObligatory();
            field.setShouldBeFilledBySender(false);
            field.authorObligatory = "signatory";
            field.addedByMe = false;
          }
        });
      }

      if (!field.isOptional() && field.canBeOptional()) {
        options.push({
          name: localization.designview.optionalField,
          onSelect: function () {
            field.makeOptional();
            field.setShouldBeFilledBySender(false);
            field.authorObligatory = "optional";
            field.addedByMe = false;
          }
        });
      }

      return options;
    },

    fontSelected: function () {
      var model = this.props.model;
      var field = model.field();
      var sig = field.signatory();
      var page = sig.document().mainfile().page(model.get("page"));

      var currSize = model.fsrel() * page.width();

      if (Math.abs(currSize - FieldPlacementGlobal.fontSizeSmall) < 1) {
        return localization.fontSize.small;
      }

      if (Math.abs(currSize - FieldPlacementGlobal.fontSizeNormal) < 1) {
        return localization.fontSize.normal;
      }

      if (Math.abs(currSize - FieldPlacementGlobal.fontSizeLarge) < 1) {
        return localization.fontSize.big;
      }

      if (Math.abs(currSize - FieldPlacementGlobal.fontSizeHuge) < 1) {
        return localization.fontSize.large;
      }

      return localization.fontSize.custom;
    },

    fontOptions: function () {
      var model = this.props.model;
      var field = model.field();
      var sig = field.signatory();
      var page = sig.document().mainfile().page(model.get("page"));

      return [
        {name: localization.fontSize.small,
         style: {fontSize: FieldPlacementGlobal.fontSizeSmall + "px"},
         onSelect: function () { model.setFSRel(FieldPlacementGlobal.fontSizeSmall / page.width()); }},
        {name: localization.fontSize.normal,
         style: {fontSize: FieldPlacementGlobal.fontSizeNormal + "px"},
         onSelect: function () { model.setFSRel(FieldPlacementGlobal.fontSizeNormal / page.width()); }},
        {name: localization.fontSize.big,
         style: {fontSize: FieldPlacementGlobal.fontSizeLarge + "px"},
         onSelect: function () { model.setFSRel(FieldPlacementGlobal.fontSizeLarge / page.width()); }},
        {name: localization.fontSize.large,
         style: {fontSize: FieldPlacementGlobal.fontSizeHuge + "px"},
         onSelect: function () { model.setFSRel(FieldPlacementGlobal.fontSizeHuge / page.width()); }}
      ];
    },

    microcopy: function () {
      var model = this.props.model;
      var field = model.field();

      if (field.isAuthorUnchangeableField()) {
        if (field.isEmail()) {
          return localization.designview.emailCanBeChangedInAccountSection;
        }
        return localization.designview.nameCanBeChangedInAccountSection;
      }
    },

    handleDone: function () {
      var model = this.props.model;
      var field = model.field();
      mixpanel.track("Click save inline field");
      field.makeReady();
      model.cleanTypeSetter();
      model.trigger("change:step");
    },

    renderTitle: function () {
      var placement = this.props.model;
      var field = placement.field();
      var fname = field.nicename();
      var signatory = placement.signatory();
      var sname = signatory.nameOrEmail() || signatory.nameInDocument();

      var copy = $("<div class='title'>" + localization.designview.requestFieldFrom + "</div>");
      $(".put-field-name", copy).text(fname);
      $(".put-person-name", copy).text(sname);
      return <span dangerouslySetInnerHTML={{__html: copy.html()}} />;
    },

    renderBody: function () {
      var model = this.props.model;
      var field = model.field();
      var sig = field.signatory();
      var page = sig.document().mainfile().page(model.get("page"));

      return (
        <span>
          {field.isAuthorUnchangeableField() &&
            <div className="microcopy"> {this.microcopy()} </div>
          }
          {!field.isAuthorUnchangeableField() &&
            <div style={{display:"block", marginBottom: "5px"}}>
              <Select.Select
                name={this.obligatorySelected()}
                options={this.obligatoryOptions()}
                optionsWidth={"218px"}
                textWidth={191}
                cssClass={"typesetter-obligatory-option"}
                style={{fontSize: "16px"}}
              />
            </div>
          }
          {!(page == undefined || page.width() == undefined) &&
            <Select.Select
              name={localization.fontSize.name + ": " + this.fontSelected()}
              options={this.fontOptions()}
              optionsWidth="218px"
              textWidth={191}
              cssClass="typesetter-obligatory-option"
              style={{fontSize: "16px"}}
            />
          }
          <Done field={field} onDone={this.handleDone} />
        </span>
      );
    }
  });
});
