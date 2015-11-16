/** @jsx React.DOM */

define(["React", "Backbone", "signview/signatories/docviewsignatory",
  "signview/signatories/docviewsignatory", "legacy_code"],
  function (React, Backbone, DocumentViewSignatory) {
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
      var sigs = this.signatories();

      sigs = [{type: "title"}].concat(sigs);

      var groups = _.groupBy(sigs, function (sig, index) {
        return Math.floor(index / 3);
      });

      return (
        <span>
          {_.map(groups, function (group, index) {
            return (
              <div key={index} className="section parties">
                {_.map(group, function (s) {
                  if (s.type === "title") {
                    return (
                      <div key="title" className="col-xs-4">
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
      );
    }
  });
});
