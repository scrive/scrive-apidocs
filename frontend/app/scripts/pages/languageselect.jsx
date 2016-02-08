/** @jsx React.DOM */


define(['React', 'Backbone', 'common/select'], function(React, Backbone, Select) {

  return React.createClass({
    propTypes: {
      langprefix : React.PropTypes.string
    },
    changeLang : function(l) {
      mixpanel.track('Click switch languages', { 'Language' : l });
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
        , {value : "et", name : localization.languages.etInEt, hidden: true}
        , {value : "lv", name : localization.languages.lvInLv, hidden: true}
        , {value : "lt", name : localization.languages.ltInLt, hidden: true}
      ],function(l) {return l.name;});
    },
    render: function() {
     var self = this;
     var visibleLanguages = _.filter(this.languages(), function(l) { return !l.hidden;});
     var options = _.filter(visibleLanguages, function(l) { return ('/' + l.value + '/') !=  self.props.langprefix });
     var lname = _.find(this.languages(), function(l) {return ('/' + l.value + '/') == self.props.langprefix}).name;
     return (
      <div className='langSwitcher'>
        <Select
          options={options}
          name={lname}
          border={this.props.border}
          className={this.props.cssClass}
          width={this.props.width}
          maxOptionsWidth={this.props.maxOptionsWidth}
          adjustHeightOnExpand={this.props.adjustHeightOnExpand}
          onSelect={function(v) {self.changeLang(v);}}
        />
      </div>
     );
    }
  });

});

