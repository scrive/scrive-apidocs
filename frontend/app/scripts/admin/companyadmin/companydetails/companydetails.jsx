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
        number: this.props.company.companynumber(),
        address: this.props.company.address(),
        zip: this.props.company.zip(),
        city: this.props.company.city(),
        country: this.props.company.country(),
        ipaddressmasklist: this.props.company.ipaddressmasklist(),
        partnerid: this.props.company.partnerid(),
        cgidisplayname: this.props.company.cgidisplayname(),
        cgiserviceid: this.props.company.cgiserviceid(),
        idledoctimeout: this.props.company.idledoctimeout(),
        allowsavesafetycopy: this.props.company.allowsavesafetycopy(),
        smsprovider: this.props.company.smsprovider(),
        padappmode: this.props.company.padappmode(),
        padearchiveenabled: this.props.company.padearchiveenabled()
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
    window.location.href = "/adminonly/companyadmin/" + newCompanyId;
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
              number={this.props.viewModel.get("number")}
              address={this.props.viewModel.get("address")}
              zip={this.props.viewModel.get("zip")}
              city={this.props.viewModel.get("city")}
              country={this.props.viewModel.get("country")}
              ipaddressmasklist={
                this.props.viewModel.get("ipaddressmasklist")
              }
              partnerid={this.props.viewModel.get("partnerid")}
              cgidisplayname={this.props.viewModel.get("cgidisplayname")}
              cgiserviceid={this.props.viewModel.get("cgiserviceid")}
              idledoctimeout={this.props.viewModel.get("idledoctimeout")}
              allowsavesafetycopy={
                this.props.viewModel.get("allowsavesafetycopy")
              }
              smsprovider={this.props.viewModel.get("smsprovider")}
              padappmode={this.props.viewModel.get("padappmode")}
              padearchiveenabled={
                this.props.viewModel.get("padearchiveenabled")
              }
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

