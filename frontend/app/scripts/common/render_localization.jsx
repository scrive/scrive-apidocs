define(["jquery", "Underscore", "React"], function ($, _, React) {
  return React.createClass({
    propTypes: {
      text: React.PropTypes.string.isRequired,
      subs: React.PropTypes.object
    },

    getDefaultProps: function () {
      return {subs: {}};
    },

    render: function () {
      var $el = $("<span />").html(this.props.text);

      _.each(this.props.subs, function (value, key) {
        $el.find("." + key).text(value);
      });

      return <span dangerouslySetInnerHTML={{__html: $el.html()}} />;
    }
  });
});
