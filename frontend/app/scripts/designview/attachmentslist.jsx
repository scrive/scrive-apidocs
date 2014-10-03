/** @jsx React.DOM */

define(['React', 'common/backbone_mixin','lists/list','legacy_code'], function(React, BackboneMixin, List) {

return React.createClass({
    addAttachment : function(title,file) {
      mixpanel.track('Select attachment from list');
      this.props.model.addAttachment(
         new DesignAuthorAttachment({
           name : title,
           serverFileId : file
        })
      );
    },
    render: function() {
      var self = this;
      return (
        <List.List
          url='/a?domain=All'
          dataFetcher={function(d) {return d.list;}}
          idFetcher={function(d) {return d.field("fields").id;}}
          ref='list'
        >
          <List.Column
            name={localization.authorattachments.selectAttachment}
            width="400px"
            rendering={function(d) {
              return (
                <a onClick={function() {self.addAttachment(d.field("fields").title,d.field("fields").file)}}>
                  {d.field("fields").title}
                </a>
              );
            }}
          />
        </List.List>
      );
    }
});


});

