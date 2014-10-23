/** @jsx React.DOM */

define(['React', 'common/backbone_mixin','common/reloadable_mixin','legacy_code', 'lists/textfiltering', 'lists/pagination','lists/listmodel', 'lists/listaction','lists/listselectaction','lists/selectfilter','lists/selectajaxfilter','lists/selectfromtofilter','lists/listfooter','lists/listheader','lists/column','lists/sublist','lists/listsubheader'], function(React, BackboneMixin, ReloadableMixin, _legacyCode,TextFiltering,Pagination,ListModel,ListAction,ListSelectActions,SelectFilter,SelectAjaxFilter,SelectFromToFilter,ListHeader,ListFooter,Column,Sublist,ListSubHeader) {


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
      var model = new ListModel({
        url : props.url,
        dataFetcher : props.dataFetcher,
        idFetcher : props.idFetcher,
        loadLater : this.props.loadLater,
        onReload : props.onReload
      });
      return {model: model};
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
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
        return c instanceof SelectFilter || c instanceof SelectAjaxFilter || c instanceof SelectFromToFilter;
      });
    },
    columns : function() {
      return _.filter(this.elements(), function(c) {
        return c instanceof Column;
      });
    },
    sublist : function() {
      return _.find(this.elements(), function(c) {
        return c instanceof Sublist;
      });
    },
    textfiltering : function() {
      return _.find(this.elements(), function(c) {
        return c instanceof TextFiltering;
      });
    },
    pagination : function() {
      return _.find(this.elements(), function(c) {
        return c instanceof Pagination;
      });
    },
    actions : function() {
      return _.filter(this.elements(), function(c) {
        return c instanceof ListAction;
      });
    },
    selectactions : function() {
      return _.find(this.elements(), function(c) {
        return c instanceof ListSelectActions;
      });
    },
    listheader : function() {
      return _.find(this.elements(), function(c) {
        return c instanceof ListHeader;
      });
    },
    listfooter : function() {
      return _.find(this.elements(), function(c) {
        return c instanceof ListFooter;
      });
    },
    listsubheader : function() {
      return _.find(this.elements(), function(c) {
        return c instanceof ListSubHeader;
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
      var hasFirstTopBar = header != undefined || selectfilters.length > 0 || (selectactions != undefined && textfiltering != undefined);
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
                    {/*if*/ (header != undefined) &&
                      (header)
                    }

                    { selectfilters.map(function(c) {
                          return React.addons.cloneWithProps(c,{model: model, key : c.props.key || Math.random()});

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
                          return React.addons.cloneWithProps(c,{model: model, key : c.props.key || Math.random()});
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
                    return React.addons.cloneWithProps(c,{model: model, key : c.props.key || Math.random()});
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
                            return React.addons.cloneWithProps(c,{data: d, key : c.props.key || Math.random()});
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
                              return (<td key={"dummy-td-" + (c.props.key || Math.random())}/>);
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
                  (React.addons.cloneWithProps(pagination,{model: model.paging()}))
              }
          </div>
        </div>
      );
    }
});


return {
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
});

