var React = require("react");

var Company = require("../../../../js/account/company.js").Company;
var CompanyDetailsViewModel = require("./companydetailsviewmodel");
var FlashMessage = require("../../../../js/flashmessages.js").FlashMessage;
var FlashMessageAfterReload = require(
  "../../../../js/flashmessages.js"
).FlashMessageAfterReload;

var ButtonBarView = require("./buttonbar");
var DetailsEditorView = require("./detailseditor");

var CompanyDetailsView = React.createClass({
  propTypes: {
    companyId: React.PropTypes.string.isRequired,
    company: React.PropTypes.instanceOf(Company).isRequired,
    viewModel: React.PropTypes.instanceOf(CompanyDetailsViewModel).isRequired
  },
  getInitialState: function () {
    return {
      ready: false
    };
  },
  componentDidMount: function () {
    this.props.viewModel.on("change", this.onViewModelChange);
    this.props.company.on("change", this.onCompanyChange);

    this.reloadData();
  },
  componentWillUnmount: function () {
    this.props.viewModel.off("change", this.onViewModelChange);
    this.props.company.off("change", this.onCompanyChange);
  },
  reloadData: function () {
    this.props.company.set({ready: false}, {silent: true});
    this.props.company.fetch({cache: false, processData: true});
  },
  onCompanyChange: function () {
    this.props.viewModel.set(
      {
        companyId: this.props.companyId,
        name: this.props.company.companyname(),
        parentid: this.props.company.parentid(),
        parentgrouppath: this.props.company.parentgrouppath(),
        companynumber: this.props.company.companynumber(),
        entityname: this.props.company.entityname(),
        address: this.props.company.address(),
        zip: this.props.company.zip(),
        city: this.props.company.city(),
        country: this.props.company.country(),
        ipaddressmasklist: this.props.company.ipaddressmasklist(),
        cgidisplayname: this.props.company.cgidisplayname(),
        cgiserviceid: this.props.company.cgiserviceid(),
        idledoctimeoutpreparation: this.props.company.idledoctimeoutpreparation(),
        idledoctimeoutclosed: this.props.company.idledoctimeoutclosed(),
        idledoctimeoutcanceled: this.props.company.idledoctimeoutcanceled(),
        idledoctimeouttimedout: this.props.company.idledoctimeouttimedout(),
        idledoctimeoutrejected: this.props.company.idledoctimeoutrejected(),
        idledoctimeouterror: this.props.company.idledoctimeouterror(),
        immediatetrash: this.props.company.immediatetrash(),
        smsprovider: this.props.company.smsprovider(),
        padappmode: this.props.company.padappmode(),
        padearchiveenabled: this.props.company.padearchiveenabled(),
        sendtimeoutnotification: this.props.company.sendtimeoutnotification(),
        totpismandatory: this.props.company.totpismandatory(),
        sessiontimeout: this.props.company.sessiontimeout(),
        portalurl: this.props.company.portalurl(),
        eidservicetoken: this.props.company.eidservicetoken(),
        addressIsInherited: this.props.company.companyaddressisinherited(),
        settingsIsInherited: this.props.company.companysettingsisinherited(),
        inheritedAddress: this.props.company.companyinheritedaddress(),
        inheritedSettings: this.props.company.companyinheritedsettings(),
        companyinternaltags: this.props.company.companyinternaltags(),
        companyexternaltags: this.props.company.companyexternaltags()
      },
      {
        silent: true
      }
    );

    this.setState({
      ready: this.props.company.ready()
    });
  },
  onViewModelChange: function () {
    this.forceUpdate();
  },
  onDetailsEditorFieldChange: function (field, newValue) {
    this.props.viewModel.set(field, newValue);
  },
  onSaveComplete: function () {
    new FlashMessage({type: "success", content: "Saved"});
    this.reloadData();
  },
  onSave: function () {
    this.props.viewModel.saveDetails().sendAjax(this.onSaveComplete);
  },
  onMergeSuccess: function (newCompanyId) {
    FlashMessageAfterReload({type: "success", content: "Merged"});
    window.location.href = "/adminonly-old/companyadmin/" + newCompanyId;
  },
  onMergeError: function () {
    new FlashMessage({type: "error", content: "Failed"});
  },
  onMerge: function (newCompanyId) {
    var self = this;

    this.props.viewModel.mergeToCompany(newCompanyId).sendAjax(
      function () {
        self.onMergeSuccess(newCompanyId);
      },
      this.onMergeError
    );
  },
  render: function () {
    return (
      <div>
        {this.state.ready &&
          <div className="tab-container account">
            <DetailsEditorView
              companyId={this.props.viewModel.get("companyId")}
              name={this.props.viewModel.get("name")}
              companynumber={this.props.viewModel.get("companynumber")}
              entityname={this.props.viewModel.get("entityname")}
              address={this.props.viewModel.get("address")}
              zip={this.props.viewModel.get("zip")}
              city={this.props.viewModel.get("city")}
              country={this.props.viewModel.get("country")}
              ipaddressmasklist={
                this.props.viewModel.get("ipaddressmasklist")
              }
              parentid={this.props.viewModel.get("parentid")}
              parentgrouppath={this.props.viewModel.get("parentgrouppath")}
              cgidisplayname={this.props.viewModel.get("cgidisplayname")}
              cgiserviceid={this.props.viewModel.get("cgiserviceid")}
              idledoctimeoutpreparation={this.props.viewModel.get("idledoctimeoutpreparation")}
              idledoctimeoutclosed={this.props.viewModel.get("idledoctimeoutclosed")}
              idledoctimeoutcanceled={this.props.viewModel.get("idledoctimeoutcanceled")}
              idledoctimeouttimedout={this.props.viewModel.get("idledoctimeouttimedout")}
              idledoctimeoutrejected={this.props.viewModel.get("idledoctimeoutrejected")}
              idledoctimeouterror={this.props.viewModel.get("idledoctimeouterror")}
              immediatetrash={this.props.viewModel.get("immediatetrash")}
              smsprovider={this.props.viewModel.get("smsprovider")}
              padappmode={this.props.viewModel.get("padappmode")}
              padearchiveenabled={
                this.props.viewModel.get("padearchiveenabled")
              }
              sendtimeoutnotification={
                this.props.viewModel.get("sendtimeoutnotification")
              }
              totpismandatory={
                this.props.viewModel.get("totpismandatory")
              }
              sessiontimeout={
                this.props.viewModel.get("sessiontimeout")
              }
              portalurl={
                this.props.viewModel.get("portalurl")
              }
              eidservicetoken={
                this.props.viewModel.get("eidservicetoken")
              }
              addressIsInherited={this.props.viewModel.get("addressIsInherited")}
              settingsIsInherited={this.props.viewModel.get("settingsIsInherited")}
              inheritedAddress={this.props.viewModel.get("inheritedAddress")}
              inheritedSettings={this.props.viewModel.get("inheritedSettings")}
              companyinternaltags={this.props.viewModel.get("companyinternaltags")}
              companyexternaltags={this.props.viewModel.get("companyexternaltags")}
              onFieldChange={this.onDetailsEditorFieldChange}
            />

            <ButtonBarView
              companyId={this.props.companyId}
              onMerge={this.onMerge}
              onSave={this.onSave}
            />
          </div>
        }
      </div>
    );
  }
});

module.exports.CompanyDetailsView = CompanyDetailsView;

module.exports.CompanyDetailsViewFactory = function (companyId) {
  var company = new Company({companyid: companyId, forAdmin: true});
  var viewModel = new CompanyDetailsViewModel();

  return (
    <CompanyDetailsView
      companyId={companyId}
      company={company}
      viewModel={viewModel}
    />
  );
};
