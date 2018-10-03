import React from "react";
import _ from "underscore";
import {Document} from "../../../js/documents";
import Track from "../../common/track";
import {ReloadManager} from "../../../js/reloadmanager.js";

import DownloadPDFIcon from "../../icons/download_pdf_icon.svg";
import DownloadZipIcon from "../../icons/download_zip_icon.svg";

module.exports = React.createClass({
  displayName: "DownloadDocumentButton",

  propTypes: {
    document: React.PropTypes.instanceOf(Document).required
  },

  onDownload: function (isZip) {
    const self = this;

    return function () {
      const doc = self.props.document;
      const sig = doc.currentSignatory();

      Track.track("Download document", {
        "Archive": isZip ? "yes" : "no",
        "Can sign": doc.currentSignatoryCanSign() ? "yes" : "no",
        "Delivery method": sig.delivery()
      });
      ReloadManager.stopBlocking();
      setTimeout(function () {
        ReloadManager.startBlocking();
      }, 1000);
    };
  },

  render: function () {
    const self = this;
    const {link, isZip} = this.props.document.downloadLink(true);

    return (
      <a className="button download"
         title={localization.docsignview.downloadDocumentButtonText}
         target="_blank"
         onClick={this.onDownload(isZip)}
         href={link}
      >
        {/* if */ isZip &&
          <DownloadZipIcon />
        }
        {/* else */ !isZip &&
          <DownloadPDFIcon />
        }
      </a>
    );
  }
});
