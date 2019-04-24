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
 * Usage with lists:
 *
 * <HtmlTextWithSubstitution
 *     secureText="<span class='people'> are super"
 *     lists={{
 *         ".people": {
 *             items: ["Bartek", "Mariusz", "Tomek"],
 *             wrapper: "<strong />",
 *             separator: ", ",
 *             lastSeparator: "and"
 *         }
 *     }}
 * />
 *
 * Params:
 *   secureText: string - we be injected as is - best if it is a text directly from localization
 *   subs - object, keys will be use to find a nodes. Text of node will be replaced with value
 *   links - as above, but will set href value
 *   classes - as above, but will add css class
 *   onClicks - as above, but will set on click handler
 *   lists - object, keys will be use to find a nodes. Value will be used to render a list
 *
 * List specification items:
 *   items: array of strings - will be used as list items
 *   wrapper: string - HTML element that will wrap the items, defaults to "<span />"
 *   separator: string - separator for all but the last items, defaults to ", "
 *   lastSeparator: string - separator put before the last item, defaults to localization.listand
 */


  module.exports = React.createClass({
    propTypes: {
      secureText: React.PropTypes.string.isRequired,
      subs: React.PropTypes.object,
      lists: React.PropTypes.object,
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

    makeList: function($container, spec) {
      var items = spec.items;
      var $separator = $("<span />").text(spec.separator || ", ");
      var $lastSeparator = $("<span />").text(
        " " + (spec.lastSeparator || localization.listand) + " "
      );

      var makeListItem = function(item) {
        var $wrapper = null;
        if (spec.wrapper) {
          $wrapper = $(spec.wrapper);
        } else {
          $wrapper = $("<span />");
        }

        $wrapper.text(item);
        return $wrapper;
      };

      if (items.length == 1) {
        $container.append(makeListItem(items[0]));
      } else {
        _.each(items.slice(0, items.length - 2), function(item, index) {
          $container.append(makeListItem(item));
          $container.append($separator.clone());
        });

        $container.append(makeListItem(items[items.length - 2]));
        $container.append($lastSeparator);
        $container.append(makeListItem(items[items.length - 1]));
      }
    },

    render: function () {
      var self = this;
      var $el = $("<span />").html(this.props.secureText);

      _.each(this.props.subs, function (value, key) {
        $el.find(key).text(value);
      });

      _.each(this.props.lists, function (value, key) {
        if (value.items && value.items.length > 0) {
          self.makeList($el.find(key), value);
        }
      });

      _.each(this.props.links, function (value, key) {
        $el.find(key).attr("href",value).attr("target", "_blank").attr("rel", "noopener noreferrer");
      });

      _.each(this.props.classes, function (value, key) {
        $el.find(key).addClass(value);
      });

      return <span className={this.props.className} dangerouslySetInnerHTML={{__html: $el.html()}} />;
    }
  });
