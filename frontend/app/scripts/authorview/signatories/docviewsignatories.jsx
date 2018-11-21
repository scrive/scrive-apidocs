var React = require("react");
var Backbone = require("backbone");
var AuthorViewAutomaticReminders = require("../authorviewautomaticreminders");
var DocumentViewSignatoryForList = require("./docviewsignatoryforlist");
var DocumentViewSignatory = require("./docviewsignatory");
var _ = require("underscore");

  module.exports = React.createClass({
    propTypes: {
      document: React.PropTypes.object,
      onAction: React.PropTypes.func
    },

    getInitialState: function () {
      return {currentIndex: 0};
    },

    setCurrentIndex: function (i) {
      this.setState({currentIndex: i});
    },

    currentIndex: function () {
      return this.state.currentIndex;
    },

    signatories: function () {
      var signatories = this.props.document.signatories();
      var current = _.find(signatories, function (s) { return s.current(); });
      var others  = _.filter(signatories, function (s) { return !s.current(); });
      var sigs = _.compact([current].concat(others));
      return sigs;
    },

    hasAutomaticReminder: function () {
      var document = this.props.document;
      var hasNotSigned = _.any(document.signatories(), function (s) {
        return (s.signs() || s.approves()) && !s.hasSigned() && !s.padDelivery();
      });

      return document.pending()
        && (document.timeouttime().diffDays() > 0 || document.autoremindtime() != undefined)
        && hasNotSigned
        && document.currentViewerIsAuthor();
    },

    hasList: function () {
      return this.signatories().length > 2;
    },

    isSingleSignatory: function () {
      return this.signatories().length == 1;
    },

    render: function () {
      var self = this;
      var document = this.props.document;
      var lastSignatoryIndex = self.hasList() ?
        self.currentIndex() : (self.isSingleSignatory() ? 0 : 1);
      var lastSignatory = self.signatories()[lastSignatoryIndex];

      return (
        <div className="signatories section" >
          <div className="column first">
            <h2 style={{width: "260px", float: "none", paddingLeft: "0px"}}>
              {localization.authorview.signatoriesTitle}
            </h2>
          </div>
          <div className="column middle">
            {/* if */ self.hasList() &&
              <div className="list spacing grey-box" style={{padding: "0px"}}>
                {self.signatories().map(function (s, i) {
                  return (
                    <DocumentViewSignatoryForList
                      key={String(s.signatoryid())}
                      signatory={s}
                      onSelect={function () { self.setCurrentIndex(i); }}
                      first= {i == 0}
                      last={i == self.signatories().length - 1}
                      active={i == self.currentIndex()}
                    />
                  );
                })}
              </div>
            }
            {/* else */ !self.hasList() && !self.isSingleSignatory() &&
              <DocumentViewSignatory
                key={self.signatories()[0].signatoryid()}
                signatory={self.signatories()[0]}
                onAction={this.props.onAction}
              />
            }
          </div>
          <div className="column last">
            <DocumentViewSignatory
              key={lastSignatory.signatoryid()}
              signatory={lastSignatory}
              onAction={this.props.onAction}
            />
            {/* if */ this.hasAutomaticReminder() &&
              <AuthorViewAutomaticReminders document={this.props.document} onAction={this.props.onAction} />
            }
          </div>
        </div>
      );
    }
  });
