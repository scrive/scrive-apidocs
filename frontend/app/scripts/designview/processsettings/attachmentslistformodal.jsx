/** @jsx React.DOM */

define(['React','lists/list','legacy_code'], function(React, List) {

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
          dataFetcher={function(d) {return d.attachments;}}
          idFetcher={function(d) {return d.field("id");}}
          ref='list'
        >
          <List.Column
            name={localization.authorattachments.selectAttachment}
            width="400px"
            rendering={function(d) {
              return (
                <a onClick={function() {self.addAttachment(d.field("title"),d.field("file"))}}>
                  {d.field("title")}
                </a>
              );
            }}
          />
        </List.List>
      );
    }
});


});

