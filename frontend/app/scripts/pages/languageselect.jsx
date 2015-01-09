/** @jsx React.DOM */


define(['React', 'Backbone', 'common/select'], function(React, Backbone, NewSelect) {

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
      ],function(l) {return l.name;});
    },
    render: function() {
     var self = this;
     var visibleLanguages = _.filter(this.languages(), function(l) { return !l.hidden;});
     var options = _.filter(visibleLanguages, function(l) { return ('/' + l.value + '/') !=  self.props.langprefix });
     var lname = _.find(this.languages(), function(l) {return ('/' + l.value + '/') == self.props.langprefix}).name;
     var Select = NewSelect.Select;
     return (
      <div className='langSwitcher'>
        <Select
          options={options}
          name={lname}
          border={this.props.border}
          cssClass={this.props.cssClass}
          textWidth={this.props.textWidth}
          optionsWidth={this.props.optionsWidth}
          adjustHeightOnExpand={this.props.adjustHeightOnExpand}
          onSelect={function(v) {self.changeLang(v);}}
        />
      </div>
     );
    }
  });

});

