var React = require("react");
var $ = require("jquery");

var Header = require("../../pages/special/header");
var Footer = require("../../pages/special/footer");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");

var ForwardedView = React.createClass({
  deliveryText: function (t) {
    if (t == "email") {
      return localization.delivery.email;
    } else if (t == "pad") {
      return localization.delivery.pad;
    } else if (t == "mobile") {
      return localization.delivery.mobile;
    } else if (t == "email_mobile") {
      return localization.delivery.email_mobile;
    } else if (t == "link") {
      return localization.delivery.link;
    }
  },

  render: function () {
    var fdata = {};
    try  {
      fdata = JSON.parse(Cookies.get("forwarded_data"));
    } catch(e) {}
    return (
      <div className="main">
        <div className="forwarded-view-body">
          <div className="title" >
            <HtmlTextWithSubstitution
              secureText={localization.forward.forwardedConfirmationTitle}
              subs={{
                ".put-document-title-here": fdata.title
              }}
            />
          </div>
          <div className="message" >
            <h1>{localization.forward.forwardedConfirmationThankYou}</h1>
            <div className="description">
              <HtmlTextWithSubstitution
                secureText={fdata.forsigning
                  ? localization.forward.forwardedConfirmationDescriptionSigning
                  : localization.forward.forwardedConfirmationDescriptionApproving
                }
                subs={{
                  ".put-signatory-name-here": fdata.name,
                  ".put-delivery-method-here": this.deliveryText(fdata.delivery_method)
                }}
              />
            </div>
            <div className="description"> {localization.forward.forwardedConfirmationClosePage} </div>
          </div>
        </div>
      </div>
    );
  }
});

$(function () {
  var documentid = fromTemplate.documentId;
  var forwardedViewBodyDiv = $("<div />");
  React.render(React.createElement(ForwardedView,{
  }), forwardedViewBodyDiv[0]);

  var headerDiv = $("<div/>");
  React.render(React.createElement(Header,{
    documentid: documentid
  }), headerDiv[0]);

  var footerDiv = $("<div/>");
  React.render(React.createElement(Footer,{
  }), footerDiv[0]);

  $(".forwarded-view").prepend(headerDiv);
  $(".mainContainer").append(forwardedViewBodyDiv);
  $(".forwarded-view").append(footerDiv);


  mixpanel.register({
    Context : "To Start"
  });
});
