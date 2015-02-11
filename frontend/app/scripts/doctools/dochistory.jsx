/** @jsx React.DOM */

define(['React', 'common/backbone_mixin','lists/list', 'moment', 'legacy_code'], function(React, BackboneMixin, List, moment, _legacyCode) {

var DocumentHistoryView =  React.createClass({
    getDefaultProps: function() {
      return {
        expaded: false
      };
    },
    getInitialState: function() {
      return this.stateFromProps(this.props);
    },
    componentWillReceiveProps: function(props) {
      this.setState(this.stateFromProps(props));
    },
    stateFromProps : function(props) {
      return {expanded: props.expanded};
    },
    setExpanded : function(b) {
      this.setState({expanded : b});
    },
    expanded : function() {
      return this.state.expanded;
    },
    expandText : function() {
      var expandString = localization.history.expand.replace(/[0-9]+/,this.refs.list.list().length - 15);
      return "+ " + expandString;
    },
    ready : function() {
      return this.refs.list != undefined && this.refs.list.ready();
    },
    hideText : function() {
      return "- " + localization.history.hide;
    },
    checkIfHistoryChangedAndCallback : function(changedcallback,errorcallback) {
      this.refs.list != undefined && this.refs.list.checkIfChangedAndCallback(changedcallback,errorcallback);
    },
    render: function() {
      var self = this;
      return (
        <div className="document-history-container">
          <List.List
            url={"/api/frontend/history/" + this.props.documentid}
            dataFetcher={function(d) {return d.list;}}
            ref='list'
            onReload={function() {self.forceUpdate();}}
            showLimit={this.state.expanded ? undefined : 15}
          >
            <List.Column
              name={localization.archive.documents.columns.status}
              width="46px"
              rendering={function(d) {
                return (
                <div className={"icon status "+d.field("fields").status}></div>
                );
              }}
            />
            <List.Column
              name={localization.history.time}
              width="150px"
              rendering={function(d) {
                return (
                  <span>
                      {/*if*/ (d.field("fields").time != undefined && d.field("fields").time != "" ) &&
                        <span>{moment(d.field("fields").time).toDate().toTimeAbrevWithMinutes()}</span>
                      }
                  </span>
                );
              }}
            />
            <List.Column
              name={localization.history.party}
              width="200px"
              rendering={function(d) {
                return (
                  <div>{d.field("fields").party}</div>
                );
              }}
            />
            <List.Column
              name={localization.history.description}
              width="460px"
              rendering={function(d) {
                return (
                <div style={{"marginRight":"30px"}} dangerouslySetInnerHTML={{__html: d.field("fields").text}}/>
                );
              }}
            />
          </List.List>
          {/* if */ (this.refs.list != undefined && this.refs.list.ready() && this.refs.list.list().length > 15) &&
            (
              <div className="document-history-footer"  onClick={function() {self.setExpanded(!self.expanded());}}>
                <div className="option">
                  <div className="label">
                    {/* if */ (!this.expanded()) &&
                      (<span> { self.expandText() } </span>)
                    }
                    {/* else */ (this.expanded()) &&
                      (<span> { self.hideText() } </span>)
                    }
                  </div>
                </div>
              </div>
            )
          }
        </div>
      );
    }
});

return function(args){
        var el = $("<div/>");
        var view = React.render(React.createElement(DocumentHistoryView,{documentid : args.document.documentid()}), el[0]);
        this.el     = function() {return el;};
        this.recall = function() { view().reload();};
        this.expanded = function() { return view.expanded();};
        this.setExpanded = function(expanded) {view.setExpanded(expanded);};
        this.ready  = function() {return view.ready()};
        this.destroy = function() { React.unmountComponentAtNode(el[0]); this.checkIfHistoryChangedAndCallback = function() {};};
        this.checkIfHistoryChangedAndCallback = function(changedcallback,errorcallback) { view.checkIfHistoryChangedAndCallback(changedcallback,errorcallback);};
};

});
