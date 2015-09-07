/** @jsx React.DOM */

define(["React", "Backbone", "signview/signatories/docviewsignatoriesmodel",
  "signview/signatories/docviewsignatory", "legacy_code"],
  function (React, Backbone, DocumentViewSignatoriesModel, SignatoryView) {
  var DocumentViewSignatoriesView = React.createClass({
    propTypes: {
      model: React.PropTypes.object
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
      var model = this.props.model;
      var signatories = model.signatories();
      var DocumentViewSignatory = SignatoryView.DocumentViewSignatory;
      var DocumentViewSignatoryForList = SignatoryView.DocumentViewSignatoryForList;
      var lastSignatoryIndex = model.hasList() ?
        self.currentIndex() : (model.isSingleSignatory() ? 0 : 1);
      var lastSignatory = model.signatories()[lastSignatoryIndex];

      return (
        <div className="signatories section" >
          <div className="column first">
            <h2 style={{width: "260px", float:"none", paddingLeft:"0px"}}>
              {localization.docsignview.signatoriesTitle}
            </h2>
          </div>
          <div className="column middle">
            {/* if */ model.hasList() &&
              <div className="list spacing grey-box" style={{padding:"0px;"}}>
                {model.signatories().map(function (s, i) {
                  return (
                    <DocumentViewSignatoryForList
                      key={String(s.signatoryid())}
                      signatory={s}
                      onSelect={function () {self.setCurrentIndex(i);}}
                      first= {i == 0}
                      last={i == model.signatories().length - 1}
                      active={i == self.currentIndex()}
                    />
                  );
                })}
              </div>
            }
            {/* else */ !model.hasList() && !model.isSingleSignatory() &&
              <DocumentViewSignatory
                signatory={model.signatories()[0]}
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

  return React.createClass({
    propTypes: {
      document: React.PropTypes.object
    },

    getInitialState: function () {
      return this.stateFromProps(this.props);
    },

    componentWillReceiveProps: function (props) {
      this.setState(this.stateFromProps(props));
    },

    stateFromProps: function (props) {
      var model = new DocumentViewSignatoriesModel({
        document: props.document
      });
      return {model: model};
    },

    currentIndex: function () {
      return this.refs.view.currentIndex();
    },

    setCurrentIndex: function (i) {
      return this.refs.view.setCurrentIndex(i);
    },

    render: function () {
      return (
        <DocumentViewSignatoriesView ref="view" model={this.state.model}/>
      );
    }
  });
});
