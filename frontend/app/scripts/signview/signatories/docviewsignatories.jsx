/** @jsx React.DOM */

define(["React", "Backbone","signview/signatories/docviewsignatoryforlist",
  "signview/signatories/docviewsignatory", "legacy_code"],
  function (React, Backbone, DocumentViewSignatoryForList ,DocumentViewSignatory) {
  return React.createClass({
    propTypes: {
      document: React.PropTypes.object
    },

    document: function () {
      return this.props.document;
    },

    signatories: function () {
      var signatories = this.document().signatories();
      var current = _.find  (signatories, function (s) { return s.current(); });
      var others  = _.filter(signatories, function (s) { return !s.current(); });
      var sigs = _.compact([current].concat(others));
      _.filter(sigs, function (s) { return s.signs(); });

      return sigs;
    },

    hasList: function () {
      return this.signatories().length > 2;
    },

    isSingleSignatory: function () {
      return this.signatories().length == 1;
    },
    getInitialState: function () {
      return {currentIndex: 0};
    },

    setCurrentIndex: function (i) {
      this.setState({currentIndex:i});
    },

    currentIndex: function () {
      return this.state.currentIndex;
    },

    render: function () {
      var self = this;
      var signatories = this.signatories();
      var lastSignatoryIndex = this.hasList() ?
        self.currentIndex() : (this.isSingleSignatory() ? 0 : 1);
      var lastSignatory = signatories[lastSignatoryIndex];

      return (
        <div className="signatories section" >
          <div className="column first">
            <h2 style={{width: "260px", float:"none", paddingLeft:"0px"}}>
              {localization.docsignview.signatoriesTitle}
            </h2>
          </div>
          <div className="column middle">
            {/* if */ this.hasList() &&
              <div className="list spacing grey-box" style={{padding:"0px;"}}>
                {signatories.map(function (s, i) {
                  return (
                    <DocumentViewSignatoryForList
                      key={String(s.signatoryid())}
                      signatory={s}
                      onSelect={function () {self.setCurrentIndex(i);}}
                      first= {i == 0}
                      last={i == signatories.length - 1}
                      active={i == self.currentIndex()}
                    />
                  );
                })}
              </div>
            }
            {/* else */ !this.hasList() && !this.isSingleSignatory() &&
              <DocumentViewSignatory
                signatory={signatories[0]}
              />
            }
          </div>
          <div className="column last">
            <DocumentViewSignatory
              signatory={lastSignatory}
            />
          </div>
        </div>
      );
    }
  });

});
