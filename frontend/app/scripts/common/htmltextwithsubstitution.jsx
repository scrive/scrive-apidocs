var $ = require("jquery");
var _ = require("underscore");
var React = require("react");
/* Usage:
 *
 * <HtmlTextWithSubstitution
 *    secureText="<span class='who'> is super"
 *    subs={{".who": "You"}}
 * />
 *
 * Params:
 *   secureText: string - we be injected as is - best if it is a text directly from localization
 *   subs - object, keys will be use to find a nodes. Text of node will be replaced with value
 *   links - as above, but will set href value
 *   classes - as above, but will add css class
 *   onClicks - as above, but will set on click handler
 */


  module.exports = React.createClass({
    propTypes: {
      secureText: React.PropTypes.string.isRequired,
      subs: React.PropTypes.object,
      links: React.PropTypes.object,
      classes: React.PropTypes.object,
      onClicks: React.PropTypes.object
    },

    getDefaultProps: function () {
      return {subs: {}, links:{}, classes:{}, onClicks: {} };
    },

    bindClickHandlers: function () {
      var node = $(this.getDOMNode());
      _.each(this.props.onClicks, function (value, key) {
        node.find(key).off("click").click(value);
      });
    },

    componentDidUpdate: function() {
      this.bindClickHandlers();
    },

    componentDidMount: function() {
      this.bindClickHandlers();
    },

    render: function () {
      var $el = $("<span />").html(this.props.secureText);

      _.each(this.props.subs, function (value, key) {
        $el.find(key).text(value);
      });

      _.each(this.props.links, function (value, key) {
        $el.find(key).attr("href",value).attr("target", "_blank");
      });

      _.each(this.props.classes, function (value, key) {
        $el.find(key).addClass(value);
      });

      return <span className={this.props.className} dangerouslySetInnerHTML={{__html: $el.html()}} />;
    }
  });
