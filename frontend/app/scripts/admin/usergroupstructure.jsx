var React = require("react");
var _ = require("underscore");
var BackboneMixin = require("../common/backbone_mixin");

var UserGroupStructure = Backbone.Model.extend({
  initialize: function (args) {
    this.url = "/adminonly/companyadmin/getstructure/" + args.companyid;
    this.fetch();
  }
});

module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    propTypes: {
      loadLater: React.PropTypes.bool,
      companyid: React.PropTypes.string
    },
    getBackboneModels: function () {
      return [this.state.userGroupStructure];
    },
    componentDidMount: function () {
    },
    getInitialState: function () {
      return {
        userGroupStructure: new UserGroupStructure({companyid : this.props.companyid}),
        initiated: false
      };
    },
    componentWillUpdate: function() {
      if (! this.state.initiated) {
        this.setState({ initiated: true });
      }
    },
    renderUserGroup: function (ugWithChildren) {
      var classNames = undefined;
      var userGroup = ugWithChildren.group;
      if (ugWithChildren.children.length > 0) {
        classNames = ["caret"];
      } else {
        classNames = ["leaf"];
      }
      return (
        <li key={"usergroup_"+userGroup.user_group_id}>
          <span className={classNames}>
             { /* if */ (userGroup.user_group_id != this.props.companyid) &&
               <a href={userGroup.user_group_id+"#structure"}>
                 {userGroup.name} ({userGroup.user_group_id})
               </a>
             }
             { /* else */ (userGroup.user_group_id == this.props.companyid) &&
               <span className="strong">
                 {userGroup.name} ({userGroup.user_group_id})
               </span>
             }
          </span>
          { /* if */ (ugWithChildren.children.length > 0) &&
            <ul>
               {_.map(ugWithChildren.children, this.renderUserGroup)}
            </ul>
          }
        </li>
      );
    },
    render: function() {
      var self = this;
      var groupStructure = this.state.userGroupStructure.get("user_group_structure");
      return (
        <div className="tab-container user-group-structure">
          { /* if */ groupStructure && this.state.initiated &&
            <ul id="topUL">
              {this.renderUserGroup(groupStructure)}
            </ul>
          }
        </div>
      );
    }
});
