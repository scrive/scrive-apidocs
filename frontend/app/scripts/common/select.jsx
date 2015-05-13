/*
Standard select boxes.

Usage:

  <Select
    name="Option 1" // name to show on select label.
    className="select-1" // extra class for select.
    color="black" // color css property.
    border="1px solid #ddd" // border css property.
    inactive={false} // is select box inactive.
    textWidth={100} // width of select button in pixels.
    optionsWidth="200px" // width of option box as css property.
    onSelect={function (v) { return true; }} // fired when an option is selected
                                             // return false to not close option box.
    onOpen={function () { return true }} // fired when option box opens
                                         // return false to not open option box.
    onRemove={function () { }} // if set adds a remove button to select box.
    options={[
      {
        name: "Option 2", // name to show in option box.
        value: 2, // value to pass to onSelect.
        onSelect: function () { }, // if set, fires when this option is selected.
        disabled: false // if true this option will not show up in option box.
      }
    ]}
  />
*/

define(["legacy_code", "React"], function (legacy_code, React) {
  var Option = React.createClass({
    render: function () {
      return (
        <li onClick={this.props.onClick}>
          <span style={this.props.style}>{this.props.name}</span>
        </li>
      );
    }
  });

  var View = React.createClass({
    render: function () {
      var parent = this.props.parent;

      var mainStyle = {border: parent.props.border, color: parent.props.color};
      if (this.props.expanded) {
        mainStyle.position = "absolute";
        mainStyle.top = $(parent.getDOMNode()).offset().top;
        mainStyle.left = $(parent.getDOMNode()).offset().left;
      } else {
        mainStyle.maxWidth = parent.props.optionsWidth;
      }

      var buttonStyle = {
        width: parent.props.textWidth + 6 + 21 + "px"
      };

      var labelStyle = {
        width: parent.props.textWidth + "px"
      };

      var optionStyle = {
        minWidth: parent.props.optionsWidth,
        border: parent.props.border
      };

      var selectClass = React.addons.classSet({
        "select": true,
        "inactive": parent.props.inactive
      });

      if (parent.props.className) {
        selectClass += " " + parent.props.className;
      }

      if (this.props.expanded) {
        selectClass += " select-exp";
      }

      mainStyle = _.extend(mainStyle, parent.props.style);
      // do not overwrite label width, like old component.
      labelStyle = _.extend({}, parent.props.style, labelStyle);

      return (
        <div
          className={selectClass}
          style={mainStyle}
          onClick={parent.handleClick}
          onMouseEnter={this.props.expanded && parent.handleEnter}
          onMouseLeave={this.props.expanded && parent.handleLeave}
        >
          <div className="select-button" style={buttonStyle}>
            <div className="select-button-left"/>
            <div className="select-button-label" style={labelStyle}>
              {parent.props.name}
            </div>
            <div className="select-button-right"/>
            {parent.props.onRemove &&
              <div ref="close" className="closer" onClick={parent.handleRemove}/>
            }
          </div>
          {this.props.expanded &&
            <ul className="select-opts" style={optionStyle}>
              {_.map(parent.activeOptions(), function (option, index) {
                return (
                  <Option
                    key={index}
                    onClick={parent.handleSelect.bind(parent, option)}
                    {...option}
                  />
                );
              })}
            </ul>
          }
        </div>
      );
    }
  });

  var Select = React.createClass({
    propTypes: {
      name: React.PropTypes.string.isRequired,
      className: React.PropTypes.string,
      textWidth: React.PropTypes.number,
      optionsWidth: React.PropTypes.string,
      color: React.PropTypes.string,
      style:  React.PropTypes.object,
      onOpen: React.PropTypes.func,
      onSelect: React.PropTypes.func,
      onRemove: React.PropTypes.func,
      options: React.PropTypes.array.isRequired,
      inactive: React.PropTypes.bool
    },

    getInitialState: function () {
      return {expanded: false};
    },

    getDefaultProps: function () {
      return {
        border: "1px solid #ddd",
        color: "black",
        style: {},
        textWidth : 160,
        optionsWidth : "200px",
        inactive: false,
        onOpen: function () { return true; }
      };
    },

    componentDidUpdate: function (prevProps, prevState) {
      if (!prevState.expanded && this.state.expanded) {
        this.mountExpanded();
      }

      if (prevState.expanded && !this.state.expanded) {
        this.unmountExpanded(true);
      }

      if (this.state.expandComponent && this.state.expandComponent.isMounted()) {
        this.state.expandComponent.forceUpdate();
      }
    },

    componentWillUnmount: function () {
      this.unmountExpanded(false);
    },

    mountExpanded: function () {
      var $expand = $("<div>");
      var component = React.render(<View expanded={true} parent={this} />, $expand[0]);
      $("body").append($expand);
      this.setState({$expand: $expand, expandComponent: component});
    },

    unmountExpanded: function (reset) {
      var $expand = this.state.$expand;
      if ($expand) {
        React.unmountComponentAtNode($expand[0]);
        $expand.remove();
        if (reset) { this.setState({$expand: null, expandComponent: null}); }
      }
    },

    open: function () {
      if (this.props.onOpen() && this.activeOptions().length > 0 && !this.props.inactive) {
        this.setState({expanded: true});
      }
    },

    close: function () {
      this.setState({expanded: false});
    },

    toggle: function () {
      if (!this.state.expanded) {
        this.open();
      } else {
        this.close();
      }
    },

    select: function (index) {
      var option = this.activeOptions()[index];
      if (option) {
        var onSelect = option.onSelect || this.props.onSelect;
        onSelect(option.value);
      }
    },

    activeOptions: function () {
      var options = _.filter(this.props.options, function (option) {
        return !option.disabled;
      });

      return options;
    },

    handleClick: function () {
      this.toggle();
    },

    handleRemove: function (e) {
      e.stopPropagation();
      this.close();
      this.props.onRemove();
    },

    handleEnter: function () {
      clearTimeout(this.state.closeTimeout);
    },

    handleLeave: function () {
      var self = this;

      if (BrowserInfo.doesNotSupportHoverPseudoclassSelector() || BrowserInfo.isPadDevice()) {
        return ;
      }

      var timeoutId = setTimeout(function () {
        self.close();
      }, 50);

      self.setState({closeTimeout: timeoutId});
    },

    handleSelect: function (option, e) {
      e.stopPropagation();

      var onSelect = option.onSelect || this.props.onSelect;

      if (onSelect) {
        var close = onSelect(option.value);

        if ((close || typeof close !== "boolean") && this.isMounted()) {
          this.close();
        }
      }
    },

    render: function () {
      return <View ref="view" parent={this} />;
    }
  });

  return Select;
});
