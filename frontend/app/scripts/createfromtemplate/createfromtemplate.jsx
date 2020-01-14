var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var Utils = require("../archive/utils");
var List = require("../lists/list");
var moment = require("moment");
var Submit = require("../../js/submits.js").Submit;
var LoadingDialog = require("../../js/loading.js").LoadingDialog;
var Modal = require("../common/modal");
var InfoTextInput = require("../common/infotextinput");
module.exports = React.createClass({
    getInitialState: function () {
      return {
        selectedTemplate: null,
        showBIPDModal: false,
        newDocumentBPID: "",
        requireBPID: window.promptbpid
      };
    },
    onPageShow: function (event) {
      if (event.persisted) {
        // If the page is loaded from the browser's cache (e.g. after the user
        // clicked the Back button), refresh the UI.
        this.refs.templatesList.reload();
        LoadingDialog.close();
      }
    },
    componentWillMount: function () {
      window.addEventListener("pageshow", this.onPageShow, false);
    },
    componentWillUnmount: function () {
      window.removeEventListener("pageshow", this.onPageShow, false);
    },
    createFromTemplate: function(id) {
      var self = this;
      if (self.state.requireBPID) {
        self.setState({selectedTemplate: id, showBIPDModal: true});
      } else {
        self.createFromTemplateAndRedirect(id);
      }
    },
    closeBIPDModal: function() {
      this.setState({selectedTemplate: null, showBIPDModal: false});
    },
    onNewDocumentBPIDChange: function (value) {
      this.setState({ newDocumentBPID: value });
    },
    acceptBIPDModal: function() {
      const bpid = this.state.newDocumentBPID;
      if (bpid.length > 0 && /^[a-z0-9]+$/i.test(bpid)) {
        this.createFromTemplateAndRedirect(this.state.selectedTemplate,bpid);
      }
    },
    createFromTemplateAndRedirect : function(id,bpid) {
      new Submit({
        method : "POST",
        url: "/api/frontend/documents/newfromtemplate/" + id,
        ajax: true,
        bpid: bpid,
        onSend: function() {
          LoadingDialog.open();
        },
        ajaxerror: function (d, a){
          if (d.status === 401) {
            // session died, relogin
            window.location = "/enter?referer=" + window.location.pathname;
          }
          LoadingDialog.close();
        },
        ajaxsuccess: function(d) {
          try {
            window.location.href = "/d/"+d.id;
          } catch(e) {
            LoadingDialog.close();
          }
        }
      }).send();
    },
    render: function() {
      var self = this;
      return (
        <div className="create-from-template">
          <List.List
            maxPageSize={Utils.maxPageSize}
            totalCountFunction={Utils.totalCountFunction}
            url={Utils.listCallUrl}
            paramsFunction={Utils.paramsFunctionWithFilter([
                {"filter_by" : "is_template"},
                {"filter_by" : "is_not_in_trash"}
              ])}
            dataFetcher={Utils.dataFetcher}
            idFetcher={Utils.idFetcher}
            loadLater={self.props.loadLater}
            ref="templatesList"
          >
          <List.ListHeader>
            <div className="headline">
              {localization.createFromTemplateDescription}
            </div>
          </List.ListHeader>

          <List.TextFiltering text={localization.archive.templates.search}/>

          <List.Column
              name={localization.archive.templates.columns.time}
              width="100px"
              sorting="mtime"
              rendering={function(d) {
                var time = moment(d.field("mtime")).toDate();
                return (<div text={time.fullTime()}>{time.toYMDString()}</div>);
              }}
            />


          <List.Column
            name={localization.archive.templates.columns.template}
            width="310px"
            sorting="title"
            rendering={function(d) {
              var id = d.field("id");
              var title = d.field("title");
              return (<a onClick={function(){self.createFromTemplate(id);return false;}}>{title}</a>);
            }}
          />
          <List.Column
            name={localization.archive.templates.columns.verificationMethod}
            width="100px"
            rendering={function(d) {
              return (<div>{Utils.documentDeliveryText(d)}</div>);
            }}
          />
          <List.Column
            name={localization.archive.templates.columns.sharedBy}
            width="102px"
            rendering={function(d) {
              return (<div>{(d.field("is_shared")) ? Utils.signatorySmartName(Utils.documentAuthor(d)) : ""}</div>);
            }}
          />

          <List.Pagination/>

          </List.List>

          <Modal.Container active={self.state.showBIPDModal}>
            <Modal.Header
              title={"Create from template"}
              showClose={true}
              onClose={self.closeBIPDModal}
            />
            <Modal.Content>
                <div className="position first">
                  <label style={{"margin-right" : "10px"}}>BusinessPartner ID</label>
                  <InfoTextInput
                    ref="newdocumentbpid"
                    inputtype="text"
                    name="newdocumentbpid"
                    value={self.state.newDocumentBPID}
                    onChange={this.onNewDocumentBPIDChange}
                  />
                </div>
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={self.closeBIPDModal} />
              <Modal.AcceptButton onClick={self.acceptBIPDModal} />
            </Modal.Footer>
          </Modal.Container>

        </div>
      );
    }
});
