var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var ReloadableMixin = require("../common/reloadable_mixin");
var TextFiltering = require("./textfiltering");
var Pagination = require("./pagination");
var ListModel = require("./listmodel");
var ListAction = require("./listaction");
var ListSelectActions = require("./listselectaction");
var SelectFilter = require("./selectfilter");
var SelectAjaxFilter = require("./selectajaxfilter");
var SelectFromToFilter = require("./selectfromtofilter");
var ListHeader = require("./listheader");
var ListFooter = require("./listfooter");
var Column = require("./column");
var Sublist = require("./sublist");
var ListSubHeader = require("./listsubheader");
var _ = require("underscore");



var List = React.createClass({
    propTypes: {
    },
    getInitialState: function() {
      return this.stateFromProps(this.props);
    },
    componentWillReceiveProps: function(props) {
      // One should never alter ListModel at props changes.
      // New ListModel will fetch data from server, and this is never what you want to do.
    },
    stateFromProps : function(props) {
      // This component will create if own model, unless ListModel is directly provided
      var model = this.props.model || new ListModel({
        url : props.url,
        dataFetcher : props.dataFetcher,
        idFetcher : props.idFetcher,
        loadLater : this.props.loadLater,
        onReload : props.onReload,
        paramsFunction : props.paramsFunction,
        maxPageSize : props.maxPageSize,
        totalCountFunction : props.totalCountFunction
      });
      return {model: model};
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      if (this.props.externalModel)
        return [];
      else
        return [this.state.model];
    },
    ready: function() {
      return this.state.model.ready();
    },
    list : function() {
      return this.state.model.list();
    },
    reload : function() {
      this.state.model.reload();
    },
    checkIfChangedAndCallback : function(changedcallback,errorcallback) {
      this.state.model.checkIfChangedAndCallback(changedcallback,errorcallback);
    },
    /* If there is only one child, children is object, not array. But rest of our code will only work with arrays*/
    elements : function() {
      return _.flatten([this.props.children]);
    },
    selectfilters : function() {
      return _.filter(this.elements(), function(c) {
        return c.type === SelectFilter.type || c.type === SelectAjaxFilter.type || c.type === SelectFromToFilter.type;
      });
    },
    columns : function() {
      return _.filter(this.elements(), function(c) {
        return c.type === Column.type;
      });
    },
    sublist : function() {
      return _.find(this.elements(), function(c) {
        return c.type === Sublist.type;
      });
    },
    textfiltering : function() {
      return _.find(this.elements(), function(c) {
        return  c.type === TextFiltering.type;
      });
    },
    pagination : function() {
      return _.find(this.elements(), function(c) {
        return c.type === Pagination.type;
      });
    },
    actions : function() {
      return _.filter(this.elements(), function(c) {
        return c.type === ListAction.type;
      });
    },
    selectactions : function() {
      return _.find(this.elements(), function(c) {
        return c.type === ListSelectActions.type;
      });
    },
    listheader : function() {
      return _.find(this.elements(), function(c) {
        return c.type === ListHeader.type;
      });
    },
    listfooter : function() {
      return _.find(this.elements(), function(c) {
        return c.type === ListFooter.type;
      });
    },
    listsubheader : function() {
      return _.find(this.elements(), function(c) {
        return c.type === ListSubHeader.type;
      });
    },
    render: function() {
      var self = this;
      var model = this.state.model;
      var columns = this.columns();
      var sublist = this.sublist();
      var textfiltering = this.textfiltering();
      var pagination = this.pagination();
      var actions = this.actions();
      var selectactions = this.selectactions();
      var selectfilters = this.selectfilters();
      var header = this.listheader();
      var footer = this.listfooter();
      var subheader = this.listsubheader();
      var showHeader = header != undefined && (header.props.availableWhen == undefined || header.props.availableWhen(model));
      var hasFirstTopBar = showHeader || selectfilters.length > 0 || (selectactions != undefined && textfiltering != undefined);
      var hasSecondTopBar =  actions.length >0 || selectactions != undefined ||  (!hasFirstTopBar && textfiltering != undefined);
      var hasAnyTopBar = hasFirstTopBar || hasSecondTopBar;
      var rows = model.list() === undefined ? [] : model.list();
      rows = this.props.showLimit === undefined ? rows : rows.slice(0, this.props.showLimit);
      return (
        <div className='list-container' style={{opacity : model.ready() ? "1" : "0.5" }}>

          {/*if*/ (hasAnyTopBar) &&
            <div className="option-top-box">

              {/*if*/ (hasFirstTopBar) &&
                <div>
                  <div className="col float-left">
                    {/*if*/ showHeader &&
                      (header)
                    }

                    { selectfilters.map(function(c) {
                          return React.addons.cloneWithProps(c,{model: model, key : c.key || Math.random()});

                        })
                    }
                  </div>
                  <div className="col float-right">
                    {/*if*/ (textfiltering != undefined) &&
                      (React.addons.cloneWithProps(textfiltering,{model: model.textfiltering()}))
                    }
                  </div>
                  <div className="clearfix"/>
                </div>
              }

              {/*if*/ (hasSecondTopBar) &&
                <div className="subbox">
                  <div className="actions-box">

                    <div>
                      { actions.map(function(c) {
                          return React.addons.cloneWithProps(c,{model: model, key : c.key || Math.random()});
                        })
                      }
                    </div>
                  </div>
                  <div className="options-box">
                    <div>
                      {/*if*/ (selectactions != undefined) &&
                        (React.addons.cloneWithProps(selectactions,{model: model}))
                      }
                    </div>
                   {/*if*/ (textfiltering != undefined && !hasFirstTopBar) &&
                        (React.addons.cloneWithProps(textfiltering,{model: model.textfiltering()}))
                   }
                  </div>
                </div>
              }
            </div>
          }

          <div className='table'>
            <table>
              <thead>
                <tr>
                { columns.map(function(c) {
                    return React.addons.cloneWithProps(c,{model: model, key : c.key || Math.random()});
                  })
                }
                </tr>
                {/*if*/ (subheader != undefined) &&
                  <tr>
                    <th className="subheadline" colSpan={columns.length}>
                      {subheader}
                    </th>
                  </tr>
                }
              </thead>
              <tbody className='selectable'>
                {/*if*/ (model.list() != undefined) &&
                  _.flatten([ rows.map(function(d) {
                    var sl = [];
                    if (sublist != undefined && sublist.props.count(d) > 0)
                    {
                      for(var i=0;i<sublist.props.count(d);i++)
                        sl[i] = React.addons.cloneWithProps(sublist,{data: d,index:i, key: Math.random() });
                    }
                    return [(
                      <tr key={"tr-" + (d.id() || Math.random())}>
                        { columns.map(function(c) {
                            return React.addons.cloneWithProps(c,{data: d, key : c.key || Math.random(), model:model});
                          })
                        }
                      </tr>),sl];
                    })
                  ])
                }
                {/*if*/ (model.list() != undefined && this.props.minRows != undefined) &&
                  ( function() {
                      var array = [];
                      for(var i =0; i< self.props.minRows - model.list().length; i++ )
                        array.push(
                          <tr key={"dummy-tr-" + i}>
                           { columns.map(function(c) {
                              return (<td key={"dummy-td-" + (c.key || Math.random())}/>);
                            })
                           }
                          </tr>
                        );
                      return array;
                  }()
                  )
                }
              </tbody>
            </table>
              {/*if*/ (footer != undefined && (footer.props.availableWhen == undefined || footer.props.availableWhen(model))) &&
                  (footer)
              }
              {/*if*/ (pagination != undefined) &&
                  (React.addons.cloneWithProps(pagination,{model: model}))
              }
          </div>
        </div>
      );
    }
});


module.exports = {
  List : List,
  TextFiltering : TextFiltering,
  Pagination : Pagination,
  ListAction : ListAction,
  ListSelectActions : ListSelectActions,
  SelectFilter : SelectFilter,
  SelectAjaxFilter : SelectAjaxFilter,
  SelectFromToFilter : SelectFromToFilter,
  ListHeader : ListHeader,
  ListFooter : ListFooter,
  ListSubHeader : ListSubHeader,
  Column : Column,
  Sublist : Sublist,
  ReloadableContainer : ReloadableMixin
};
