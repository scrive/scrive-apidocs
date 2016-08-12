var React = require("react");
var Backbone = require("backbone");
var DocumentViewSignatory = require("./docviewsignatory");
var ViewSize = require("../viewsize");
var Document = require("../../../js/documents.js").Document;
var _ = require("underscore");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Document).isRequired
    },

    signatories: function () {
      var signatories = this.props.model.signatories();
      var current = _.find(signatories, function (s) { return s.current(); });
      var others  = _.filter(signatories, function (s) { return !s.current(); });
      var sigs = _.compact([current].concat(others));
      return _.filter(sigs, function (s) { return s.signs(); });
    },

    render: function () {
      var sigs = this.signatories();

      if (!ViewSize.isSmall()) {
        sigs = [{type: "title"}].concat(sigs);
      }

      var numGroups = ViewSize.isMedium() ? 2 : 3;

      var groups = _.groupBy(sigs, function (sig, index) {
        return Math.floor(index / numGroups);
      });

      return (
        <span>
          {/* if */ !ViewSize.isSmall() &&
            <span>
              {_.map(groups, function (group, index) {
                return (
                  <div key={index} className="section parties">
                    {_.map(group, function (s) {
                      if (s.type === "title") {
                        return (
                          <div key="title" className={ViewSize.isMedium() ? "col-xs-6" : "col-xs-4"}>
                            <h1 className="title">{localization.docsignview.signatoriesTitle}</h1>
                          </div>
                        );
                      } else {
                        return <DocumentViewSignatory key={s.signatoryid()} signatory={s} />;
                      }
                    })}
                  </div>
                );
              })}
            </span>
          }
          {/* else */ ViewSize.isSmall() &&
            <span>
              {_.map(sigs, function (s, index) {
                return <DocumentViewSignatory first={index == 0} key={s.signatoryid()} signatory={s} />;
              })}
            </span>
          }
        </span>
      );
    }
  });
