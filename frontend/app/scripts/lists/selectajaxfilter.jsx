/** @jsx React.DOM */

define(['React','common/select'], function(React,NewSelect) {


return React.createClass({
    propTypes: {
      name : React.PropTypes.string,
      defaultName : React.PropTypes.string,
      optionsURL : React.PropTypes.string,
      optionsParse : React.PropTypes.func
    },
    getInitialState: function() {
      return {ajaxOptions : undefined};
    },
    getMoreDataIfNeededAndOpen : function() {
      var self = this;
      if (this.state.ajaxOptions != undefined) {
        return true;
      }
      else {
        $.ajax(
          this.props.optionsURL,
          {
            cache: false,
            success : function(res) {
              var options = self.props.optionsParse(JSON.parse(res));
              self.setState({ajaxOptions : options});
              self.open();
            }
          }
        );
        return false;
      }
    },
    open : function() {
      this.refs.select.open();
    },
    render: function() {
      var self = this;
      var model = self.props.model;
      var options = [{name : this.props.defaultName, value : ""}];
      if (this.state.ajaxOptions != undefined)
        options = options.concat(this.state.ajaxOptions);
      var selectedValue = model.selectfiltering().filteringValue(self.props.name);
      var availableOptions = [];

      var selectedOptionName = "";
      for(var i=0;i< options.length;i++) {
         if (options[i].value != selectedValue) {
           availableOptions.push(options[i]);
         } else {
           selectedOptionName = options[i].name;
         }

      }
      var Select = NewSelect.Select;
      return (
        <div className='float-left'>
          <Select
            ref='select'
            color={"#000000"}
            options={availableOptions}
            name ={selectedOptionName}
            textWidth={this.props.textWidth}
            onOpen={this.getMoreDataIfNeededAndOpen}
            onSelect={function(value) {
                      var selectedOption = _.find(options,function(o) {return o.value == value;});
                      mixpanel.track('Filter ' + self.props.name,
                                    {Value : selectedOption.name});
                      model.selectfiltering().setFilter(self.props.name, value);
                      return true;
            }}
         />
       </div>
      );
    }
});

});

