var React = require("react");
var _ = require("underscore");

var BackboneMixin = require("../../common/backbone_mixin");
var FilePageView = require("./filepageview");

var File = require("../../../js/files.js").File;

module.exports = React.createClass({
  mixins: [BackboneMixin.BackboneMixin],
  propTypes: {
    model: React.PropTypes.instanceOf(File).isRequired,
    onReady: React.PropTypes.func.isRequired
  },
  getBackboneModels: function () {
    return [this.props.model];
  },
  getInitialState: function () {
    return {
      ready: false
    };
  },
  componentWillMount: function () {
    this._pageRefs = [];

    if (!this.props.model.ready()) {
      this.props.model.fetch({
        data: {signatoryid: this.props.model.signatoryid()},
        processData: true,
        cache: false
      });
    }
  },
  componentDidUpdate: function () {
    if (this.ready()) {
      this.props.onReady();
    }
  },
  ready: function () {
    return this.state.ready;
  },
  allPagesRendered: function () {
    return (this._pageRefs.length == this.props.model.pages().length);
  },
  allPagesReady: function () {
    return _.all(this._pageRefs, function (item) {
      return item.ready();
    });
  },
  refreshReady: function () {
    this.setState({
      ready: (this.props.model.ready() && this.allPagesRendered() && this.allPagesReady())
    });
  },
  addPageRef: function (page) {
    if (page && this._pageRefs.indexOf(page) == -1) {
      this._pageRefs.push(page);
    }
  },
  onPageReady: function (page) {
    this.refreshReady();
  },
  render: function () {
    var self = this;

    return (
      <div className="document-pages">
        { /* if */ (!this.props.model.ready()) &&
          <div className="waiting4page"></div>
        }
        { /* else */ (this.props.model.ready()) &&
          _.map(this.props.model.pages(), function (item, index) {
            return (
              <FilePageView
                key={index}
                model={item}
                ref={(page) => self.addPageRef(page)}
                onReady={self.onPageReady}
              />
            );
          })
        }
      </div>
    );
  }
});
