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
      if (this.authorIsViewer()) {
        var author  = _.find(signatories, function (s) { return s.author(); });
        var others1 = _.filter(signatories, function (s) { return !s.author() && !s.current() && s.signs(); });
        return _.compact([author, current].concat(others1));
      } else {
        var others2 = _.filter(signatories, function (s) { return !s.current() && s.signs(); });
        return _.compact([current].concat(others2));
      }
    },
    authorIsViewer: function () {
      var signatories = this.props.model.signatories();
      var author = _.find(signatories, function (s) { return s.author(); });
      return !author.signs();
    },

    render: function () {
      var sigs = this.signatories();
      var authorIsViewer = this.authorIsViewer();

      var numGroups = ViewSize.isMedium() ? 2 : 3;

      var groups = _.groupBy(sigs, function (sig, index) {
        return Math.floor(index / numGroups);
      });

      return (
        <span>
          {/* if */ !ViewSize.isSmall() &&
            <span>
              {_.map(groups, function (group, gIndex) {
                return (
                  <div key={gIndex} className="section parties">
                    {_.map(group, function (s, sIndex) {
                      var authorTitle = authorIsViewer && sIndex == 0 && gIndex == 0;
                      var firstPartyTitle = gIndex == 0 &&
                            ((authorIsViewer && sIndex == 1) || (!authorIsViewer && sIndex == 0));
                      var titleRow = gIndex == 0 && !authorTitle && !firstPartyTitle;
                      return (
                        <DocumentViewSignatory
                            isViewingAuthor={authorTitle}
                            firstParty={firstPartyTitle}
                            titleRow={titleRow}
                            key={s.signatoryid()} signatory={s}
                        />
                      );
                    })}
                  </div>
                );
              })}
            </span>
          }
          {/* else */ ViewSize.isSmall() &&
            <span>
              {_.map(sigs, function (s, index) {
                var authorTitle = authorIsViewer && index == 0;
                var firstPartyTitle = ((authorIsViewer && index == 1) || (!authorIsViewer && index == 0));
                return <DocumentViewSignatory
                            isViewingAuthor={authorTitle}
                            firstParty={firstPartyTitle}
                            key={s.signatoryid()} signatory={s} />;
              })}
            </span>
          }
        </span>
      );
    }
  });
