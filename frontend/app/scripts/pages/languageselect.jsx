var React = require("react");
var Backbone = require("backbone");
var Select = require("../common/select");
var Track = require("../common/track");
var Language = require("../../js/utils/language.js").Language;
var _ = require("underscore");



  module.exports = React.createClass({
    propTypes: {
      langprefix : React.PropTypes.string
    },
    changeLang : function(l) {
      Track.track('Click switch languages', { 'Language' : l });
      Language.changeForPageAndReload(l);
    },
    languages : function() {
      return _.sortBy([
          {value : "en", name : localization.languages.enInEn}
        , {value : "sv", name : localization.languages.svInSv}
        , {value : "de", name : localization.languages.deInDe}
        , {value : "fr", name : localization.languages.frInFr}
        , {value : "it", name : localization.languages.itInIt}
        , {value : "es", name : localization.languages.esInEs}
        , {value : "pt", name : localization.languages.ptInPt}
        , {value : "nl", name : localization.languages.nlInNl}
        , {value : "da", name : localization.languages.daInDa}
        , {value : "no", name : localization.languages.noInNo}
        , {value : "el", name : localization.languages.elInEl}
        , {value : "fi", name : localization.languages.fiInFi}
        , {value : "is", name : localization.languages.isInIs}
        , {value : "et", name : localization.languages.etInEt}
        , {value : "lv", name : localization.languages.lvInLv}
        , {value : "lt", name : localization.languages.ltInLt}
        , {value : "cs", name : localization.languages.csInCs}
        , {value : "pl", name : localization.languages.plInPl}
        , {value : "hu", name : localization.languages.huInHu}
      ],function(l) {return l.name;});
    },
    render: function() {
     var self = this;
     return (
      <div className='langSwitcher'>
        <Select
          options={this.languages()}
          isOptionSelected={function(l) {
            return ('/' + l.value + '/') ==  self.props.langprefix;
          }}
          className={this.props.cssClass}
          width={this.props.width}
          onSelect={function(v) {self.changeLang(v);}}
        />
      </div>
     );
    }
  });
