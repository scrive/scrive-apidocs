var React = require("react");
var _ = require("underscore");

var BackboneMixin = require("../../common/backbone_mixin");
var FilePageView = require("./filepageview");

var Document = require("../../../js/documents.js").Document;

module.exports = React.createClass({
  mixins: [BackboneMixin.BackboneMixin],
  propTypes: {
    model: React.PropTypes.instanceOf(Document).isRequired,
    onReady: React.PropTypes.func.isRequired
  },
  getBackboneModels: function () {
    return [this.props.model, this.props.model.mainfile()];
  },
  getInitialState: function () {
    return {
      ready: false
    };
  },
  componentWillMount: function () {
    this._pageRefs = [];

    if (this.props.model.mainfile()) {
      if (!this.props.model.mainfile().ready()) {
        this.props.model.mainfile().fetch({
          data: {signatoryid: this.props.model.mainfile().signatoryid()},
          processData: true,
          cache: false
        });
      }
    }
  },
  componentDidUpdate: function () {
    if (this.ready()) {
      this.props.onReady();
    } else if (this.props.model.closed() && this.props.model.mainfile() && !this.props.model.mainfile().ready()) {
      // For some reason, if the document is closed, sealed file isn't
      // properly reloaded when refreshing the author view. This should do
      // the trick.
      this.props.model.mainfile().fetch();
    }
  },
  ready: function () {
    return this.state.ready;
  },
  allPagesRendered: function () {
    return (this._pageRefs.length == this.props.model.mainfile().pages().length);
  },
  allPagesReady: function () {
    return _.all(this._pageRefs, function (item) {
      return item.ready();
    });
  },
  refreshReady: function () {
    this.setState({
      ready: (this.props.model.mainfile().ready() && this.allPagesRendered() && this.allPagesReady())
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
    var doc = self.props.model;

    return (
      <div className="document-pages">
        { /* if */ (!this.props.model.mainfile() || !this.props.model.mainfile().ready()) &&
          <div className="waiting4page"></div>
        }
        { /* else */ (this.props.model.mainfile() && this.props.model.mainfile().ready()) &&
          _.map(this.props.model.mainfile().pages(), function (page, index) {
            return (
              <FilePageView
                key={index}
                page={page}
                highlightedPages={doc ? [] : doc.noneditableHighlighedPagesForPageNo(page.number())}
                ref={(p) => self.addPageRef(p)}
                onReady={self.onPageReady}
              />
            );
          })
        }
      </div>
    );
  }
});
