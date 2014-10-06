/** @jsx React.DOM */


define(['React', 'Backbone', 'authorview/authorviewautomaticreminders','doctools/docviewsignatory', 'legacy_code'], function(React, Backbone, Reminders, SignatoryView) {

var expose = {};


var DocumentViewSignatoriesModel = Backbone.Model.extend({
  defaults : function() { return {
     onAction : function() {},
     forSigning : false
    };
  },
  initialize: function (args) {
  },
  forSigning : function() {
    return this.get("forSigning");
  },
  signatories: function() {
        var signatories = this.document().signatories();
        var current = _.find  (signatories, function(s) { return  s.current(); });
        var others  = _.filter(signatories, function(s) { return !s.current(); });
        var sigs = _.compact([current].concat(others));
        if (this.forSigning())
          return _.filter(sigs, function(s) { return s.signs(); });
        else
          return sigs;

  },
  onAction : function() {
    return this.get("onAction");
  },
  hasAutomaticReminder : function() {
      return !this.forSigning()
             && this.document().pending()
             && (this.document().timeouttime().diffDays() > 0 || this.document().autoremindtime() != undefined)
             && _.any(this.document().signatories(), function(s) { return s.signs() && !s.hasSigned() && !s.padDelivery(); })
             && this.document().currentViewerIsAuthor();
  },
  document :function() {
     return this.get("document");
  },
  hasList : function() {
     return this.signatories().length > 2;
  },
  isSingleSignatory : function() {
     return this.signatories().length ==1;
  }
});


var DocumentViewSignatoriesView = React.createClass({
    propTypes: {
      model: React.PropTypes.object
    },
    getInitialState: function() {
      return {
        currentIndex : 0
      };
    },
    setCurrentIndex : function(i) {
      this.setState({currentIndex :i});
    },
    currentIndex : function() {
      return this.state.currentIndex;
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      var signatories = model.signatories();
      var AuthorViewAutomaticReminders = Reminders.AuthorViewAutomaticReminders;
      var DocumentViewSignatory = SignatoryView.DocumentViewSignatory;
      var DocumentViewSignatoryForList = SignatoryView.DocumentViewSignatoryForList;

      return (
        <div className='signatories section' >
          <div className='column first'>
            <h2 style={{width: "260px", float:"none", paddingLeft:"0px"}}>
             {model.forSigning() ? localization.docsignview.signatoriesTitle : localization.authorview.signatoriesTitle }
            </h2>
          </div>
          <div className='column middle'>

              {/*if*/ model.hasList() &&
                <div className='list spacing grey-box' style={{padding:"0px;"}}>
                  {model.signatories().map(function(s,i) {
                    var status = s.status();
                    return (
                      <DocumentViewSignatoryForList
                        key = {s.signatoryid() + ""}
                        signatory = {s}
                        onSelect =  {function() {self.setCurrentIndex(i);}}
                        first  = {i == 0}
                        last   = {i == model.signatories().length - 1}
                        active = {i == self.currentIndex()}
                      />
                      );
                    })
                  }
                </div>
              }
              {/*else*/ !model.hasList() && !model.isSingleSignatory() &&
                <DocumentViewSignatory
                  signatory  = {model.signatories()[0]}
                  onAction   = {model.onAction()}
                  forSigning = {model.forSigning()}
                />
              }

          </div>
          <div className='column last'>
                <DocumentViewSignatory
                  signatory  = {model.signatories()[model.hasList() ? self.currentIndex() : (model.isSingleSignatory() ? 0 : 1)]}
                  onAction   = {model.onAction()}
                  forSigning = {model.forSigning()}
                />

              {/*if*/ model.hasAutomaticReminder()&&
                <AuthorViewAutomaticReminders document={model.document()} onAction={model.onAction()} />
              }
          </div>
       </div>


      );
    }
  });





var DocumentViewSignatories = React.createClass({
    propTypes: {
      document: React.PropTypes.object,
      forSigning: React.PropTypes.bool,
      onAction: React.PropTypes.func
    },
    getInitialState: function() {
      return this.stateFromProps(this.props);
    },
    componentWillReceiveProps: function(props) {
      this.setState(this.stateFromProps(props));
    },
    stateFromProps : function(props) {
      var model = new DocumentViewSignatoriesModel({
        document: props.document,
        forSigning : props.forSigning,
        onAction : props.onAction
      });
      return {model: model};
    },
    currentIndex : function() {
      return this.refs.view.currentIndex();
    },
    setCurrentIndex : function(i) {
      return this.refs.view.setCurrentIndex(i);
    },
    render: function() {
      return (
        <DocumentViewSignatoriesView model={this.state.model} ref='view'/>
      );
    }
  });

expose.DocumentViewSignatories = DocumentViewSignatories;

return expose;

});
