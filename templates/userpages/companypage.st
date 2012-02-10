####
viewCompany =
<div id="tab-viewer">
  <div class="tab-header">
  </div>
  <ul class="tabs">
      <li><a href="/account">$_HeadUserInfo()$</a></li>
      <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
      <li class="active"><a href="/account/company">$_CompanySettings()$</a></li>
      <li><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>
      <li><a href="/account/mailapi">$_HeadMailApi()$</a></li>
      <li><a href="/account/usagestats">$_HeadUsageStats()$</a></li>
  </ul>
  <div class="tab-container">
      <div class="tab-content account" style="height:330px;">
      </div>
  </div>
</div>
<script>

window.CompanyModel = Backbone.Model.extend({
    defaults: {
      id: 0,
      name: "",
      address: "",
      zip: "",
      city: "",
      country: "",
      barsbackground: "",
      iscustombarsbackground: false,
      ready: false,
      editable: false,
    },
    initialize: function(args) {
      this.url = "/account/company/json";
      this.fetch({cache: false});
    },
    id: function() {
      return this.get("id");
    },
    name: function() {
      return this.get("name");
    },
    address: function() {
      return this.get("address");
    },
    zip: function() {
      return this.get("zip");
    },
    city: function() {
      return this.get("city");
    },
    country: function() {
      return this.get("country");
    },
    barsbackground: function() {
      return this.get("barsbackground");
    },
    setBarsbackground: function(val) {
      this.set({"barsbackground": val.trim()});
    },
    isCustomBarsbackground: function() {
      return this.get("iscustombarsbackground");
    },
    setIsCustomBarsBackground: function(val) {
      this.set({"iscustombarsbackground": val});
    },
    isReady: function() {
      return this.get("ready");
    },
    isEditable: function() {
      return this.get("editable");
    },
    parse: function(args) {
      var isbarsbackground = args.company.barsbackground.trim()!="";
      return {
        id: args.company.id,
        name: args.company.name,
        number: args.company.number,
        address: args.company.address,
        zip: args.company.zip,
        city: args.company.country,
        barsbackground: isbarsbackground ? args.company.barsbackground : "#212121",
        iscustombarsbackground: isbarsbackground,
        editable: args.company.editable,
        ready: true
      };
    }
});

window.CompanyView = Backbone.View.extend({
  model: CompanyModel,
  initialize: function(args) {
    _.bindAll(this, "render");
    this.model.bind("change", this.render);
    this.model.view = this;
    this.initElems();
    this.render();
  },
  render: function() {
    var company = this.model;

    this.title.text(company.name());

    if (this.bbinput.val()!=company.barsbackground()) {
      this.bbinput.val(company.barsbackground());
    }

    if (company.isEditable()) {
      this.bbcheckbox.removeAttr("disabled");
    } else {
      this.bbcheckbox.attr("disabled", true);
    }

    if (company.isEditable()) {
      this.bbinput.removeAttr("disabled");
    } else {
      this.bbinput.attr("disabled", true);
    }

    if (company.isEditable()) {
      this.saveButton.show();
    } else {
      this.saveButton.hide();
    }

    this.bbdisplay.css("background-color", company.barsbackground());

    if (company.isCustomBarsbackground()) {
      this.bbcheckbox.attr("checked", true);
      this.bbcustomdiv.show();
    } else {
      this.bbcheckbox.removeAttr("checked");
      this.bbcustomdiv.hide();
    }
  },
  initElems: function() {
    var company = this.model;

    this.title = jQuery("<h2 />");
    var header = jQuery("<div class='account-header' />").append(this.title);

    var body = jQuery("<div class='account-body' />");
    var table = jQuery("<table />");
    body.append(table);
    var tablebody = jQuery("<tbody />");
    table.append(tablebody);

    var bbcheckbox = jQuery("<input id='bbcheckbox' type='checkbox' style='width: 30px;' />");
    this.bbcheckbox = bbcheckbox;
    this.bbcheckbox.change(function() {
      company.setIsCustomBarsBackground(bbcheckbox.is(":checked"));
    });
    var bbcheckboxlabel = jQuery("<label for='bbcheckbox'>Custom Email Colour</label>");

    var bbinput = jQuery("<input type='text' class='float-left' style='width: 150px; margin-left: 30px' />");
    this.bbinput = bbinput;
    this.bbinput.bind("keyup change", function() {
      company.setBarsbackground(bbinput.val());
    });

    this.bbdisplay = jQuery("<span class='float-left' style='width: 0px'/>");
    this.bbdisplay.css("margin-left", "20px");
    this.bbdisplay.css("margin-top", "5px");
    this.bbdisplay.css("padding", "10px 14px");
    this.bbdisplay.css("border", "solid 1px black");
    this.bbdisplay.css("background-color", company.barsbackground());

    this.bbcustomdiv = jQuery("<div />");
    this.bbcustomdiv.append(this.bbinput);
    this.bbcustomdiv.append(this.bbdisplay);

    var bbstuff = jQuery("<div/>");
    bbstuff.append(this.bbcheckbox);
    bbstuff.append(bbcheckboxlabel);
    bbstuff.append(this.bbcustomdiv);

    var tr1 = jQuery("<tr/>").append(jQuery("<td colspan='2' style='width:400px'/>").append(bbstuff));
    tablebody.append(tr1);

    var firstcol = jQuery("<div class='col' />");
    firstcol.append(header);
    firstcol.append(body);

    this.saveButton = Button.init({
      color: "green",
      size: "small",
      text: "Save Settings",
      onClick: function() {
        if (!company.isCustomBarsbackground()) {
          company.setBarsbackground("");
        }
        var companyjson = JSON.stringify(company);
        new Submit({
          method: "POST",
          url: "/account/company",
          company: companyjson
        }).send();
      }
    }).input();

    var cols = jQuery("<div style='height:300px;' />");
    cols.append(firstcol);

    this.el.empty();
    this.el.append(cols);
    this.el.append(jQuery("<div class='float-right'/>").append(this.saveButton));

    return this;
  }
});

window.CompanyComponent = {
  init: function(args) {
    var model = new CompanyModel();
    var div = jQuery("<div/>");
    var view = new CompanyView({ model: model, el: div});
    return new Object({
      input: function() { return div; }
    });
  }
};

var company = CompanyComponent.init();
jQuery(".tab-content").append(company.input);
</script>
#####