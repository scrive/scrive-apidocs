/** @jsx React.DOM */

define(["React", "Backbone", "signview/signatories/docviewsignatory", "signview/is_small_view",
  "signview/is_medium_view"],
  function (React, Backbone, DocumentViewSignatory, isSmallView, isMediumView) {
  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Document).isRequired
    },

    signatories: function () {
      var signatories = this.props.model.signatories();
      var current = _.find  (signatories, function (s) { return s.current(); });
      var others  = _.filter(signatories, function (s) { return !s.current(); });
      var sigs = _.compact([current].concat(others));
      return _.filter(sigs, function (s) { return s.signs(); });
    },

    render: function () {
      var smallView = isSmallView();
      var mediumView = isMediumView();
      var sigs = this.signatories();

      if (!smallView) {
        sigs = [{type: "title"}].concat(sigs);
      }

      var numGroups = mediumView ? 2 : 3;

      var groups = _.groupBy(sigs, function (sig, index) {
        return Math.floor(index / numGroups);
      });

      return (
        <span>
          {/* if */ !smallView &&
            <span>
              {_.map(groups, function (group, index) {
                return (
                  <div key={index} className="section parties">
                    {_.map(group, function (s) {
                      if (s.type === "title") {
                        return (
                          <div key="title" className={mediumView ? "col-xs-6" : "col-xs-4"}>
                            <h1 className="title">{localization.docsignview.signatoriesTitle}</h1>
                          </div>
                        );
                      } else {
                        return <DocumentViewSignatory key={String(s.signatoryid())} signatory={s} />;
                      }
                    })}
                  </div>
                );
              })}
            </span>
          }
          {/* else */ smallView &&
            <span>
              {_.map(sigs, function (s, index) {
                return <DocumentViewSignatory first={index == 0} key={String(s.signatoryid())} signatory={s} />;
              })}
            </span>
          }
        </span>
      );
    }
  });
});
