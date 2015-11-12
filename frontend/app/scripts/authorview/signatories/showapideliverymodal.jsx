/** @jsx React.DOM */

define(["legacy_code", "React", "Backbone", "common/infotextinput", "utils/location"],
  function (Legacy, React, Backbone, InfoTextInput, LocationUtils) {

  var ShowAPIDeliveryModalView = React.createClass({

    propTypes: {
      signatory: React.PropTypes.object.isRequired
    },

    getAPIDeliveryURL: function () {
      var signatory = this.props.signatory;
      return LocationUtils.origin() + signatory.signlink();
    },

    selectText: function () {
      this.refs.input.focus();
      this.refs.input.selectText();
    },

    render: function () {
      var signatory = this.props.signatory;

      return (
        <div>
          <label>{localization.docview.showAPIDelivery.description}</label>
          <InfoTextInput
            className="api-delivery-url"
            inputtype="text"
            ref="input"
            readonly={true}
            disabled={false}
            value={this.getAPIDeliveryURL()}
            onClick={this.selectText}
          />
        </div>
      );
    }
  });

  return function (args) {
    var content = $("<div class='docview-showapidelivery-modal'>");

    var contentComp = React.render(React.createElement(ShowAPIDeliveryModalView, {
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

    // Text selection needs to happend after input is appended somewhere in body
    contentComp.selectText();
  }
});
