/** @jsx React.DOM */

/*
  Standard select boxes used by our system. Rewritten in react.
  Usage:

   <Select
      name : "Option 1"              // Name on main button
      cssClass* : "select-1"         // Class to be added
      expandSide* : "right"          // What direction should it expand ("left" or "right")
      textWidth* : "100px"           // Unexpanded selectbox will not get much longer then given value. default "160px"
      optionsWidth* : "200px"        // How long should be an expanded area
      onOpen* : function(){}         // Function to be called when box gets opened. Usefull when wanting to get options with AJAX (ask M for example)
      color* : "red"                 // Color of text
      border* : "1px solid red"      // Border style
      style* :  "font-size :16px"     // Extra style applied to main label
      onSelect* : function(v) {}     // Function to be called when value is selected. If can be overwitten by onSelect from options. If true is returned select will be closed.
      onRemove* : function(v) {}     // If provided, remove (x) icon will be displayed over select. Functon will be executed when this icon will be clicked
      options: [
                      { name : "Option 1" // Name on option label
                        value* : "1" // Value that will be propagated to onSelect
                        onSelect* : function(v) {} // Function to be called on selection. It overwrites top level function
                        style* : {fontSize: 8px"} // Extra styling of label
                        disabled : false // If set to tru, option will not be visible
                      },
                      { name : "Option 2"
                        value* : "2"
                        onSelect* : function(v) {}
                      },
                  ]
   />

  Component does not expose any addtional methods. If you feel like you need some extra function like open or setName, you are probably wrong.

  Details:
    - It does not use <select> tag internally.
    - On expand, speciall diff is appended to  of current el with expanded options is displayed over current position.
      This way we can have selects withing scrollable areas, that don't make this areas expand.
    - onSelect function must be provided either for whole select or for all options
    - Select box will be closed on selection only if onSelect for selected option will return true.
*/

define(['React','common/backbone_mixim','Backbone', 'legacy_code'], function(React,BackboneMixin) {

var expose = {};

// Model for individual options
var SelectOptionModel = Backbone.Model.extend({
  defaults : {
      onSelect : function(){return false;},
      style : {},
      disabled : false
  },
  name : function(){
       return this.get("name");
  },
  value : function(){
       return this.get("value");
  },
  style: function() {
       return this.get("style");
  },
  selected : function() {
    var closeAfter = this.get("onSelect")(this.value());
    if (closeAfter == true)
      this.trigger("close");
  },
  disabled : function() {
      return this.get("disabled");
  }
});

// Model for whole select
var SelectModel = Backbone.Model.extend({
  defaults : {
      name  : "",
      options : [],
      expandSide : "left",
      expanded : false,
      onOpen : function() {return true;},
      color: "black",
      border: "1px solid #ddd",
      textWidth : "160px",
      optionsWidth : "200px",
      style : {},
      cssClass : ""

  },
  initialize: function(args){
      var model = this;
      //Change static options to SelectOptionModel. Propagate onSelect function if it's not provided for option
      var options = _.map(args.options,function(e) {
                        e.onSelect = e.onSelect || args.onSelect;
                        var option = new SelectOptionModel(e);
                        model.listenTo(option,"close", function() {model.set({"expanded" : false});});
                        return option;
                    });
      this.set({"options" : options});
  },
  options: function(){
      return this.get("options");
  },
  activeOptions : function() {
      return _.filter(this.options(),function(o) {return !o.disabled();});
  },
  name : function(){
       return this.get("name");
  },
  setName: function(name) {
      this.set({name:name});
      return this;
  },
  style : function(){
       return this.get("style");
  },
  expanded : function(){
       return this.get("expanded");
  },
  expandSide : function() {
       return this.get("expandSide");
  },
  textWidth : function(){
       return this.get("textWidth");
  },
  optionsWidth : function() {
       return this.get("optionsWidth");
  },
  unexpand: function() {
       this.set({"expanded" : false});
  },
  expand : function() {
     if (!this.expanded() && this.onOpen() && this.options().length > 0)
       this.set({"expanded" : true});
  },
  onOpen : function(){
       if (this.get("onOpen") != undefined)
        return this.get("onOpen")();
       return true;
  },
  onRemove : function(){
       if (this.get("onRemove") != undefined)
        return this.get("onRemove")();
       return true;
  },
  hasRemoveOption : function(){
       return this.get("onRemove") != undefined;
  },
  color : function() {
     return this.get("color");
  },
  border : function() {
     return this.get("border");
  },
  cssClass : function() {
     return this.get("cssClass");
  },
  style : function() {
     return this.get("style");
  }
});

/* View for expanded box*/
var SelectExpandedView = React.createClass({
    handleRemove : function() {
      var model = this.props.model;
      model.unexpand();
      model.onRemove();
      return false;
    },
    handleExpand : function() {
      var model = this.props.model;
      model.unexpand();
      return false;
    },
    render: function() {
      var model = this.props.model;
      var mainStyle = _.extend({color: model.color(),border : model.border()},model.style());
      var labelStyle = _.extend({width: model.textWidth()},model.style());
      var optionStyle = {border:model.border(), minWidth:model.optionsWidth()};
      return (
        <div className={'select select-exp ' + model.cssClass()}  style={mainStyle}>
          <div className='select-button' onClick={this.handleExpand}>
            <div className='select-button-left'/>
            <div className='select-button-label' style={labelStyle}>
              {model.name()}
            </div>
            <div className='select-button-right'/>
            {/*if*/ model.hasRemoveOption()&&
                <div className='closer' onClick={this.handleRemove}/>
              }
          </div>

          <ul className={'select-opts '+ model.expandSide()} style={optionStyle}>
            {_.map(model.activeOptions(),function(o,i) {
              return (
                <li key={i} onClick={function() {o.selected()}}>
                  <span style={model.style()}>
                    {o.name()}
                  </span>
                </li>
              )
            })
            }
          </ul>
        </div>
      )
    }
});


/* View for not expanded box. On expand original box will be placed above it (z-index)*/

var SelectView = React.createClass({
    componentWillUnmount : function() {
        this.state.expandedComponent.unmountComponent(); //We need to clear this component on unmount for garbage collection
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    getInitialState: function() {
      var self = this;
      // Temporary div. It will be stored in local state and appended to <body> on expantion
      // It contains SelectExpandedView inside.
      var div = $("<div class='select-expanded-wrapper'/>")
                  .mouseenter(function() {self.handleMouseEnter();})
                  .mouseout(function() {self.handleMouseOut();});
      var component = React.renderComponent(
            SelectExpandedView({model : this.props.model})
            , div[0]);
      return {
        expandedDiv : div,
        expandedComponent : component
      }
    },
    closeIfNeeded : function() {
        var model = this.props.model;
        if (
          model.expanded() &&
          new Date().getTime() - this.state.enterdate > 50 &&
          ($(":hover", this.state.expandedDiv).size() == 0) &&
          (!BrowserInfo.doesNotSupportHoverPseudoclassSelector() && !BrowserInfo.isPadDevice())
          )
          model.unexpand();
    },
    handleMouseEnter : function() {
      this.state.enterdate = new Date().getTime();
    },
    handleMouseOut : function() {
      var self = this;
      setTimeout(function() {self.closeIfNeeded();}, 100);
    },
    handleRemove : function() {
      var model = this.props.model;
      model.unexpand();
      model.onRemove();
      return false;
    },
    handleExpand : function() {
      var model = this.props.model;
      model.expand();
      return false;
    },
    render: function() {
      var model = this.props.model;

      if (model.expanded()) {
        this.state.expandedDiv.css('left',$(this.getDOMNode()).offset().left + "px")
        this.state.expandedDiv.css('top',$(this.getDOMNode()).offset().top + "px")
        $('body').append(this.state.expandedDiv);
      }
      else
        this.state.expandedDiv.detach();

      return (
        <div className={'select ' + model.cssClass()}  style={_.extend({color: model.color(),border : model.border()},model.style())}
             onMouseEnter={this.handleMouseEnter}
             onMouseOut={this.handleMouseOut}
             >
          <div className='select-button' onClick={this.handleExpand}>
            <div className='select-button-left'/>
            <div className='select-button-label' style={_.extend({width: model.textWidth()},model.style())}>
              {model.name()}
            </div>
            <div className='select-button-right'/>
            {/*if*/ model.hasRemoveOption()&&
                <div className='closer' onClick={this.handleRemove}/>
              }
          </div>
        </div>
      )
    }
});


/*Interface for selectbox*/
var Select = React.createClass({
    propTypes: {
      name : React.PropTypes.string.isRequired,
      cssClass : React.PropTypes.string,
      expandSide : React.PropTypes.string,
      textWidth : React.PropTypes.string,
      optionsWidth : React.PropTypes.string,
      color : React.PropTypes.string,
      border : React.PropTypes.string,
      style :  React.PropTypes.string,
      onOpen :React.PropTypes.func,
      onSelect : React.PropTypes.func,
      onRemove : React.PropTypes.func,
      options: React.PropTypes.array.isRequired
    },
    getInitialState: function() {
      return this.stateFromProps(this.props);
    },
    componentWillReceiveProps: function(props) {
      this.setState(this.stateFromProps(props));
    },
    stateFromProps : function(props) {
        var model = new SelectModel({
        name : props.name,
        cssClass : props.cssClass,
        expandSide : props.expandSide,
        textWidth : props.textWidth,
        optionsWidth : props.optionsWidth,
        color : props.color,
        border : props.border,
        style :  props.style,
        onOpen :props.onOpen,
        onSelect : props.onSelect,
        onRemove : props.onRemove,
        options: props.options
      })
      return {model: model};
    },
    render: function() {
      return (
        <SelectView model={this.state.model}/>
      );
    }
  });


expose.Select = Select;

return expose;

});
