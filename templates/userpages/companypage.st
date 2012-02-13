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
      <div class="tab-content account" style="height:360px;">
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
      logo: "",
      iscustomlogo: false,
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
    logo: function() {
      return this.get("logo");
    },
    isCustomLogo: function() {
      return this.get("iscustomlogo");
    },
    setIsCustomLogo: function(val) {
      this.set({"iscustomlogo": val});
    },
    isReady: function() {
      return this.get("ready");
    },
    isEditable: function() {
      return this.get("editable");
    },
    submitUrl: function() {
      return this.get("url");
    },
    parse: function(args) {
      var isbarsbackground = args.company.barsbackground.trim()!="";
      var islogo = args.company.logo!="";
      return {
        id: args.company.id,
        name: args.company.name,
        number: args.company.number,
        address: args.company.address,
        zip: args.company.zip,
        city: args.company.country,
        barsbackground: args.company.barsbackground,
        iscustombarsbackground: isbarsbackground,
        logo: args.company.logo,
        iscustomlogo: islogo,
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

    this.title.text("$_EmailBranding()$");

    var bbcolour = (company.isCustomBarsbackground() && company.barsbackground()!="") ? company.barsbackground() : "#212121";
    if (this.bbinput.val()!=bbcolour) {
      this.bbinput.val(bbcolour);
    }
    this.bbdisplay.css("background-color", bbcolour);
    this.logodisplaywrapper.css("background-color", bbcolour);

    var logo = (company.isCustomLogo() && company.logo()!="") ? company.logo() : "/img/email-logo.png";
    this.logodisplay.css("background-image", "url('" + logo + "')");

    if (company.isEditable()) {
      this.bbcheckbox.removeAttr("disabled");
      this.bbinput.removeAttr("disabled");
      this.saveButton.show();
      this.logocheckbox.removeAttr("disabled");
      this.logoupload.show();
    } else {
      this.bbcheckbox.attr("disabled", true);
      this.bbinput.attr("disabled", true);
      this.saveButton.hide();
      this.logocheckbox.attr("disabled", true);
      this.logoupload.hide();
    }

    if (company.isCustomBarsbackground()) {
      this.bbcheckbox.attr("checked", true);
      this.bbcustomdiv.show();
    } else {
      this.bbcheckbox.removeAttr("checked");
      this.bbcustomdiv.hide();
    }

    if (company.isCustomLogo()) {
      this.logocheckbox.attr("checked", true);
      this.logocustomdiv.show();
    } else {
      this.logocheckbox.removeAttr("checked");
      this.logocustomdiv.hide();
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
    var bbcheckboxlabel = jQuery("<label for='bbcheckbox'>$_CustomiseColour()$</label>");

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

    var logocheckbox = jQuery("<input id='logocheckbox' name='islogo' type='checkbox' style='width: 30px;' />");
    this.logocheckbox = logocheckbox;
    this.logocheckbox.change(function() {
      company.setIsCustomLogo(logocheckbox.is(":checked"));
    });
    var logocheckboxlabel = jQuery("<label for='logocheckbox'>$_CustomiseLogo()$</label>");

    var logodisplay = jQuery("<div style='width:100%;height:33px;background-repeat:no-repeat;' />");
    var logodisplaywrapper = jQuery("<div style='margin:10px 30px;padding: 10px;' />")
    logodisplaywrapper.append(logodisplay);
    this.logodisplay = logodisplay;
    this.logodisplaywrapper = logodisplaywrapper;

    var logoupload = UploadButton.init({
      width: 150,
      name: "logo",
      text: "$_SelectImageLabel()$",
      submitOnUpload: true,
      showLoadingDialog: false,
      type: "image/png",
      submit: new Submit({
          method: "POST",
          url: company.submitUrl(),
          company: JSON.stringify(company),
          islogo: true,
          beforeSend: function() {
            //horrible hack from emily
            jQuery("input[name='company']").val(JSON.stringify(company));
            jQuery("input[name='islogo']").val(company.isCustomLogo());
          },
          ajax: true,
          onSend: function() {
            console.log("sending");
            logodisplaywrapper.css("background-color", "transparent");
            logodisplay.css("background-image", "url('/theme/images/wait30trans.gif')");
          },
          ajaxsuccess: function(d) {
            company.fetch({cache: false});
            company.change();
            logodisplay.css("background-image", "url('" + company.logo() + "')");
          },
          ajaxerror: function(d, a) {
            company.fetch({cache: false});
            logodisplay.css("background-image", "url('" + company.logo() + "')");
            console.log("error");
          }
      })
    }).input();
    this.logoupload = logoupload;

    this.logocustomdiv = jQuery("<div />");
    this.logocustomdiv.append(jQuery("<div style='padding-left:30px;padding-right:30px;padding-top:5px;'/>").append(logoupload));
    this.logocustomdiv.append(this.logodisplaywrapper);

    var logostuff = jQuery("<div/>");
    logostuff.append(this.logocheckbox);
    logostuff.append(logocheckboxlabel);
    logostuff.append(this.logocustomdiv);

    var tr2 = jQuery("<tr/>").append(jQuery("<td colspan='2' style='width:400px'/>").append(logostuff));
    tablebody.append(tr2);

    var firstcol = jQuery("<div class='col' />");
    firstcol.append(header);
    firstcol.append(body);

    this.saveButton = Button.init({
      color: "green",
      size: "small",
      text: "$_SaveSettings()$",
      onClick: function() {
        if (!company.isCustomBarsbackground()) {
          company.setBarsbackground("");
        }
        var companyjson = JSON.stringify(company);
        var islogo = company.isCustomLogo();
        new Submit({
          method: "POST",
          url: "/account/company",
          company: companyjson,
          islogo: islogo
        }).send();
      }
    }).input();

    var cols = jQuery("<div style='height:330px;' />");
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