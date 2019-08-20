var classNames = require("classnames");
var React = require("react");
var _ = require("underscore");

var BodyView = require("./bodyview");
var CSVSignatoryDesignModel = require("./csvsignatorydesignmodel");
var Document = require("../../../../js/documents.js").Document;
var Field = require("../../../../js/fields.js").Field;
var Modal = require("../../../common/modal");
var Signatory = require("../../../../js/signatories.js").Signatory;
var UploadButton = require("../../../common/uploadbutton");

var CSVSignatoryDesignModal = React.createClass({
  WIDTH_WITH_DATA: 1018,
  WIDTH_WITHOUT_DATA: 530,
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    document: React.PropTypes.instanceOf(Document).isRequired,
    setParticipantDetail: React.PropTypes.func.isRequired,
    onClose: React.PropTypes.func.isRequired
  },
  componentWillMount: function () {
    this._model = null;
  },
  componentWillReceiveProps: function (nextProps) {
    if (this.props.active != nextProps.active) {
      if (nextProps.active) {
        this.createModel();
      }
    }
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (prevProps.active != this.props.active) {
      if (!this.props.active) {
        this.destroyModel();
      }
    }
  },
  componentWillUnmount: function () {
    this.destroyModel();
  },
  createModel: function () {
    this.destroyModel();

    var csvSignatory = this.csvSignatory();
    var csv = [];

    if (csvSignatory) {
      csv = csvSignatory.csv();
    }

    this._model = new CSVSignatoryDesignModel({
      header: _.first(csv),
      rows: _.rest(csv)
    });
    this._model.on("change", this.onModelChange);
  },
  destroyModel: function () {
    if (this._model) {
      this._model.off("change", this.onModelChange);
      this._model = null;
    }
  },
  csvSignatory: function () {
    var csvSignatories = _.filter(
      this.props.document.signatories(),
      function (signatory) {
        return signatory.isCsv();
      }
    );

    if (csvSignatories) {
      return csvSignatories[csvSignatories.length - 1];
    }

    return undefined;
  },
  modalWidth: function () {
    if (this._model && this._model.hasData()) {
      return this.WIDTH_WITH_DATA;
    }

    return this.WIDTH_WITHOUT_DATA;
  },
  headerToFields: function () {
    var self = this;
    return _.map(self._model.header(), function (header, index) {
      return (
        self._model.csvstandardheaders[header] || {type: "text", name: header}
      );
    });
  },
  onAcceptButtonClick: function () {
    var fields = this.headerToFields();

    var csvSignatory = this.csvSignatory();
    if (csvSignatory) {
      for (var i = 0; i < fields.length; i++) {
        if (!csvSignatory.hasField(fields[i].type, fields[i].order, fields[i].name)) {
          csvSignatory.addField(
            new Field(_.extendOwn(fields[i], {signatory: csvSignatory}))
          );
        }
      }

      csvSignatory.setCsv(this._model.csv());
    } else {
      var signatory = new Signatory({
        document: this.props.document,
        fields: fields,
        signs: true,
        csv: this._model.csv()
      });

      this.props.document.addExistingSignatory(signatory);
      this.props.setParticipantDetail(signatory);
    }

    this.props.onClose();
  },
  onCSVUploadComplete: function (input) {
    this._model.upload(input);
  },
  onModelChange: function () {
    this.forceUpdate();
  },
  render: function () {
    var bodyClassName = classNames("designCSVSignatoryPopupContent", {
      "file-uploaded": (this._model && !this._model.isEmpty())
    });

    var subtitleText = localization.csv.subtitle;
    if (this._model && _.isArray(this._model.rows()) && this._model.rows().length > 0) {
      subtitleText = (
        this._model.rows().length + " " +
        localization.designview.participantsInFile
      );
    }

    return (
      <Modal.Container
        ref="container"
        active={this.props.active}
        width={this.modalWidth()}
        onHide={this.onHide}
      >
        <Modal.Header
          showClose={true}
          title={localization.csv.title}
          onClose={this.props.onClose}
        />
        <Modal.Content>
          {this._model &&
            <div ref="contentView" className={bodyClassName}>
              <div className="modal-subtitle centered">{subtitleText}</div>

              {!this._model.isEmpty() &&
                <BodyView model={this._model} />
              }

              <div className="buttonbox">
                <UploadButton
                  color="black"
                  fileType=".csv"
                  name="csv"
                  size="big"
                  style={{fontSize: "15px", fontWeight: "300"}}
                  text={localization.csv.selectFile}
                  width={280}
                  onUploadComplete={this.onCSVUploadComplete}
                />
              </div>
            </div>
          }
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onClose} />

          {this._model && this._model.ready() &&
            <Modal.AcceptButton
              ref="acceptModalButton"
              onClick={this.onAcceptButtonClick}
            />
          }
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = CSVSignatoryDesignModal;
