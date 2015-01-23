/** @jsx React.DOM */

/*
 * Modal for changing signatory's detail (like email or mobile) address
 *
 * Uses initial value pass in args.value
 * Does some input validation and flash message errors
 *
 * Example usage:
 *    new ChangeSignatoryDetailModal({
 *      signatory : ___,
 *      onAction : ___
 *    });
 *
 */
define(['React','common/backbone_mixin','Backbone','common/infotextinput', 'legacy_code'], function(React, BackboneMixin, Backbone, InfoTextInput) {

var ChangeSignatoryDetailModalModel = Backbone.Model.extend({
  initialize: function (args) {
    var self = this;
    self.set({signatoryDetailValue: args.signatoryDetailValue,
              validator: args.validator});
  },
  validator: function() {
    return this.get('validator');
  },
  triggerOnAction: function() {
    this.get('onAction')(this.signatoryDetailValue());
  },
  signatoryDetailValue: function() {
    return this.get('signatoryDetailValue');
  },
  setSignatoryDetailValue: function (value) {
    this.set({signatoryDetailValue: value});
  },
  isSignatoryDetailValueValid: function() {
    return this.validator().validateData(this.signatoryDetailValue());
  }
});


var ChangeSignatoryDetailModalView = React.createClass({
  mixins: [BackboneMixin.BackboneMixin],
  propTypes: {model: React.PropTypes.object,
              label:  React.PropTypes.string,
              placeholder:  React.PropTypes.string
  },
  getBackboneModels: function() {
    return [this.props.model];
  },
  render: function() {
   var model = this.props.model;
   return (
     <div>
       <label>
         <div>{this.props.label}</div>
       </label>
       <InfoTextInput
         infotext = {this.props.placeholder}
         value = {model.signatoryDetailValue()}
         onChange = {function(v) {model.setSignatoryDetailValue(v);}}
         className = {!model.isSignatoryDetailValueValid() ? 'obligatory-input' : undefined}
         inputtype = {'text'}
         autocomplete = {false}
         focus={true}
       />
     </div>
   );
  }
});

return function(args) {
  var model = new ChangeSignatoryDetailModalModel({signatoryDetailValue: args.value,
                                                  validator: args.validator,
                                                  onAction: args.onAction});
  var content = $('<div class="docview-changeauthentication-modal">');

  React.renderComponent(ChangeSignatoryDetailModalView({model: model,
                                                        label: args.label,
                                                        placeholder: args.placeholder}),
                        content[0]);
  new Confirmation({
    title: args.title,
    acceptText: args.acceptButton,
    content: content,
    width: 420,
    onAccept: function() {
      if (model.isSignatoryDetailValueValid()) {
        model.triggerOnAction();
        return true;
      } else {
        new FlashMessage({content: args.invalidValueFlash,
                          type: 'error'});
        return false; 
      }
    }
  });
}
});
