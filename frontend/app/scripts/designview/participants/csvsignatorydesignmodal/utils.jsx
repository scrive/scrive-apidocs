var headerName = function (header) {
  switch (header) {
    case "fstname":
      return localization.fstname;

    case "sndname":
      return localization.sndname;

    case "email":
      return localization.email;

    case "sigco":
      return localization.company;

    case "sigpersnr":
      return localization.personalNumber;

    case "sigcompnr":
      return localization.companyNumber;

    case "mobile":
      return localization.phone;

    default:
      return header;
  }
};

module.exports = {
  headerName: headerName
};
