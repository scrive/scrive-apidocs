/** @jsx React.DOM */

define(["legacy_code", "React", "Backbone", "common/infotextinput"],
  function (Legacy, React, Backbone, InfoTextInput) {

  var ShowAPIDeliveryModalView = React.createClass({
    getBackboneModels: function () {
      return [this.props.signatory];
    },

    propTypes: {
      signatory: React.PropTypes.object.isRequired
    },

    getAPIDeliveryURL: function () {
      var signatory = this.props.signatory;
      return window.location.origin + signatory.signlink();
    },

    selectAllOnClick: function (e, t) {
      var input = t.children[0];
      input.focus();
      input.setSelectionRange(0, input.value.length);
    },

    render: function () {
      var signatory = this.props.signatory;

      return (
        <div>
          <label>{localization.docview.showAPIDelivery.description}</label>
          <InfoTextInput
            className="api-delivery-url"
            inputtype="text"
            readonly={true}
            value={this.getAPIDeliveryURL()}
            onClick={this.selectAllOnClick}
          />
        </div>
      );
    }
  });

  return function (args) {
    var content = $("<div class='docview-showapidelivery-modal'>");

    React.render(React.createElement(ShowAPIDeliveryModalView, {
      signatory: args.signatory
    }), content[0]);

    new Confirmation({
      title: localization.docview.showAPIDelivery.title,
      closeVisible: true,
      acceptText: localization.docview.showAPIDelivery.accept,
      cancelVisible: false,
      content: content,
      width: 420
    });

    var input = $(".api-delivery-url input", content)[0];
    input.focus();
    input.setSelectionRange(0, input.value.length);
  }
});
