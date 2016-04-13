var React = require("react");
var Select = require("../common/select");
var $ = require("jquery");
var _ = require("underscore");



module.exports = React.createClass({
    propTypes: {
      name : React.PropTypes.string,
      defaultOption : React.PropTypes.object,
      optionsURL : React.PropTypes.string,
      optionsParse : React.PropTypes.func
    },
    getInitialState: function() {
      return {ajaxOptions : undefined};
    },
    componentDidMount :function() {
      this.getMoreDataIfNeeded();
    },
    componentDidUpdate :function() {
      this.getMoreDataIfNeeded();
    },
    getMoreDataIfNeeded : function() {
      var self = this;
      if (this.state.ajaxOptions != undefined || self.fetchingStarted || !this.props.model.ready()) {
        return true;
      }
      else {
        self.fetchingStarted = true;
        $.ajax(
          this.props.optionsURL,
          {
            cache: false,
            success : function(res) {
              var options = self.props.optionsParse(JSON.parse(res));
              self.setState({ajaxOptions : options});
            }
          }
        );
        return false;
      }
    },
    render: function() {
      var self = this;
      var model = self.props.model;
      var options = [this.props.defaultOption];
      if (this.state.ajaxOptions != undefined) {
        options = options.concat(this.state.ajaxOptions);
      }
      var selectedValue = model.selectfiltering().filteringValue(self.props.name);

      return (
        <div className='float-left'>
          <Select
            ref='select'
            isOptionSelected={function (o) {
              return o.value == selectedValue;
            }}
            options={options}
            width={this.props.width}
            onSelect={function(value) {
                      var selectedOption = _.find(options,function(o) {return o.value == value;});
                      mixpanel.track('Filter ' + self.props.name,
                                    {Value : selectedOption.name});
                      model.selectfiltering().setFilter(self.props.name, value);
            }}
         />
       </div>
      );
    }
});
