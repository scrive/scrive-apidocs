var React = require("react");
var Button = require("../../common/button");
var List = require("../../lists/list");
var DesignAuthorAttachment = require("./designviewattachment");

  module.exports = React.createClass({
      addAttachment: function (title, file, attachmentid) {
        mixpanel.track("Select attachment from list");
        this.props.model.addAttachment(
          new DesignAuthorAttachment({
            name: title,
            serverFileId: file,
            attachmentid: attachmentid
          })
        );
        this.props.onAdd();
      },
      render: function () {
        var self = this;
        return (
          <List.List
            url="/a?domain=All"
            dataFetcher={function (d) { return d.attachments; }}
            idFetcher={function (d) { return d.field("id"); }}
            ref='list'
          >
            <List.Column
              name={localization.authorattachments.selectAttachment}
              width="400px"
              rendering={function (d) {
                return (
                  <span>{d.field("title")}</span>
                );
              }}
            />
            <List.Column
              name=""
              width="150px"
              rendering={function (d) {
                return (
                  <Button
                    type="action"
                    size="tiny"
                    text={localization.authorattachments.add}
                    className="add-attachment"
                    onClick={function () {
                      self.addAttachment(d.field("title"), d.field("file"), d.field("id"));
                    }}
                  />
                );
              }}
            />
          </List.List>
        );
      }
  });
