var Backbone = require("backbone");
var React = require("react");
var UploadButton = require("../../scripts/common/uploadbutton");
var _ = require("underscore");
var Submit = require("../submits.js").Submit;
var $ = require("jquery");
var Confirmation = require("../confirmations.js").Confirmation;
var Signatory = require("../signatories.js").Signatory;
var Field = require("../fields.js").Field;

/*Popup when someone want to select a csv signatory. At the end it can set signatory csv.
 */

var CsvProblem = Backbone.Model.extend({
      defaults : {
      row : undefined,
      cell : undefined,
      header : false,
      description : "Some problem"
  },
  row: function() {
     return this.get("row");
  },
  cell: function() {
     return this.get("cell");
  },
  header: function() {
    return this.get('header');
  },
  aboutCell: function(r,c)
  {
     return this.row() == r && this.cell() == c;
  },
  aboutRow : function(r)
  {
     return this.row() == r && this.cell() == undefined;
  },
  description: function() {
     return this.get("description");
  },
  generalProblem: function() {
       return this.row() == undefined && this.cell() == undefined;
  }
});

 var CsvSignatoryDesign = Backbone.Model.extend({
  defaults : {
      problems : []
  },
  problems : function() {
     return this.get("problems");
  },
  ready : function() {
           return (this.rows() != undefined) && (this.rows().length > 0) &&  (this.problems().length  == 0);
  },
  header: function() {
     return this.get("header");
  },
  rows : function() {
     return this.get("rows");
  },
  csv: function() {
     return [this.header()].concat(this.rows());
  },
  maxRowLength : function() {
    var max = 0;
    _.each(this.rows(),function(r){
         max = (max > r.length) ? max : r.length;
    });
    return max;
  },
  isEmpty : function() {
     return ((this.header() == undefined || this.rows() ==  undefined)) &&  (this.problems().length  == 0);
  },
  hasData: function() {
     return this.header() != undefined || this.rows() != undefined;
  },
  problemWithRow : function(r) {
      var res;
      _.each(this.problems(),function(p) {
          if (p.aboutRow(r))
              res = p;
        });
      return res;
  },
  problemWithCell : function(r,c) {
      var res;
      _.each(this.problems(),function(p) {
          if (p.aboutCell(r,c))
              res = p;
        });
      return res;
  },
  generalProblems : function() {
      var res = new Array();
      _.each(this.problems(),function(p) {
          if (p.generalProblem())
              res.push(p);
        });
      return res;
  },
  csvstandardheaders : {
    'fstname' : { type: "name", order: 1},
    'sndname' : { type: "name", order: 2},
    'email'   : { type: "email"},
    'mobile'  : { type: "mobile"},
    'sigco'   : { type: "company"},
    'sigpersnr': { type: "personal_number"},
    'sigcompnr': { type: "company_number"}
  },
  csvSignatoryHasTextFieldWithName: function (csvSignatory, name) {
    var fieldDescr = this.csvstandardheaders[name];
    if (fieldDescr === undefined) {
      return csvSignatory.hasTextFieldWithName(name);
    } else {
      return csvSignatory.hasField(fieldDescr.type, fieldDescr.order, undefined);
    }
  },
  upload : function(input) {
       var self = this;
       var submit = new Submit({ url : "/parsecsv", method : "POST"});
       submit.addInputs(input);
       submit.sendAjax(function (resp) {
           var problems = [];
           if (resp.parseError)
              problems.push(new CsvProblem({description: localization.csv.formatError,
                                            header: true}));
           if(BlockingInfo && resp.rows && BlockingInfo.shouldBlockDocs(resp.rows.length)) {
               problems.push(new CsvProblem({description:BlockingInfo.csvMessage(resp.rows.length)}));
           }
           if (resp.header == undefined || resp.header.length < 3) {
              problems.push(new CsvProblem({description: localization.csv.atLeast3Columns}));
           }
           if (resp.length < 2) {
              problems.push(new CsvProblem({description: localization.csv.atLeast1Party}));
           }
           if (resp.header != undefined) {
              for(var i=0;i<resp.header.length;i++)
                resp.header[i] = _.keys(self.csvstandardheaders)[i] || resp.header[i];
           }
           self.set({'header': resp.header , 'rows': resp.rows, 'problems': problems });
           self.trigger("change");
      });
  }
});

/*
 */
var CsvSignatoryDesignView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model = args.model;
        this.render();
    },
    generalProblemsList : function() {
       var model = this.model;
       var box = $("<div class='generalProblems'>");
       _.each(model.generalProblems(), function(p) {
          var problem = $("<div class='problem'>").html(p.description());
          if (p.header()) {
            problem.addClass('problem-header');
          }
          box.append(problem);
        });
       return box;
    },
    headerName : function(name) {
       if (name == "fstname")
                return localization.fstname;
       if (name == "sndname")
                return localization.sndname;
       if (name == "email")
                return localization.email;
       if (name == "sigco" )
                return localization.company;
       if (name == "sigpersnr")
                return localization.personalNumber;
       if (name == "sigcompnr"  )
                return localization.companyNumber;
       if (name == "mobile")
                return localization.phone;
       return name;

    },
    dataTable : function() {
      var model = this.model;
      var table = $("<table class='csvDataTable'/>");
      var thead = $("<thead/>");
      var tbody = $("<tbody/>");
      table.append(thead).append(tbody);
      var fieldnames = this.model.header();

      if (!this.model.header()) return $('<div />');

      for(var i=0;i<this.model.header().length;i++)
         thead.append($("<th />").text(this.headerName(this.model.header()[i])));


      var rows = model.rows();
      for (var i =0 ; i< rows.length; i++)
      {
         var tr = $("<tr>");
         for (var j = 0 ; j< model.maxRowLength(); j++)
         {
           var td = $("<td>");
           td.append($("<div>").text(rows[i][j]));
             if (model.problemWithCell(i,j) != undefined)
                td.append($("<div class='problem'>").text(model.problemWithCell(i,j).description()));

           tr.append(td);
         }
         if (model.problemWithRow(i) != undefined)
         {
           var td = $("<td>");
           td.append($("<div class='problem'>").text(model.problemWithRow(i).description()));
           tr.append(td);
         }
         tbody.append(tr);
      }
      return table;
    },
    uploadButton : function(){
      var model = this.model;
      var div = $("<div class='buttonbox'/>");
      React.render(React.createElement(UploadButton,{
        name: "csv",
        color : "black",
        width: 280,
        size: "big",
        text: localization.csv.selectFile,
        fileType: ".csv",
        style: {fontSize: "15px", fontWeight: "300"},
        onUploadComplete : function(input) {
          model.upload($(input));
        }
      }), div[0]);
      return div;
    },
    render: function () {
        var view = this;
        var model = this.model;
        this.container = $(this.el);
        this.container.addClass("designCSVSignatoryPopupContent");
        this.container.empty();
        if (model.isEmpty()) {
            this.container.append(this.uploadButton());
        }
        else {
            var scrollable = $('<div class="designCSVSignatoryPopupScroll" />');
            this.container.addClass('file-uploaded');
            scrollable.append(this.generalProblemsList());
            scrollable.append(this.dataTable());
            this.container.append(scrollable);
            this.container.append(this.uploadButton());

            // Make a slightly ugly thing here...
            if (model.rows()) {
                var participants = model.rows().length;
                // TODO rework this so languages that don't have the number in the beginning of the sentence can be translated.
                $('.modal-subtitle').text(participants + ' ' + localization.designview.participantsInFile);
            } else {
                $('.modal-subtitle').text(localization.csv.subtitle); // reset to the normal text.
            }
        }
        return this;
    }
});


var CsvSignatoryDesignPopup = exports.CsvSignatoryDesignPopup =  function(args) {
         var csv = [];
         var csvSignatory = undefined;
         var designview = args.designview;
         var document = designview.document();
         _.each(document.signatories(), function(s) {
           if (s.isCsv()) {
             csv = s.csv();
             csvSignatory = s;
           }
         });

         var model = new CsvSignatoryDesign({ header: _.first(csv),  rows : _.rest(csv)  });
         var view = new CsvSignatoryDesignView({model : model, el : $("<div/>")});
         var popup = new Confirmation({
              content: $("<div>").append($("<div class='modal-subtitle centered'>").html(localization.csv.subtitle)).append($(view.el)),
              title  : localization.csv.title,
              width: model.ready() && model.hasData() ? 1018 : 530,
              acceptVisible : model.ready(),
              onAccept : function() {
                  if (csvSignatory == undefined) {
                    var fields = [];
                    for(var i = 0; i < model.header().length; i ++) {
                      var f = model.csvstandardheaders[model.header()[i]] || {type: "text", name : model.header()[i]};
                      fields.push(f);
                    }
                    var signatory = new Signatory({
                      document : document,
                      fields : fields,
                      signs : true,
                      csv : model.csv()
                    });
                    document.addExistingSignatory(signatory);
                    designview.setParticipantDetail(signatory);
                  } else {
                    for(var i = 0; i < model.header().length; i ++)
                      if (!model.csvSignatoryHasTextFieldWithName(csvSignatory, model.header()[i])) {
                        f = model.csvstandardheaders[model.header()[i]] || {type: "text", name : model.header()[i]};
                        csvSignatory.addField(new Field(_.extend(f, {signatory: csvSignatory})));
                      }

                    csvSignatory.setCsv(model.csv());
                  }

                  return true;
            }
        });
        model.bind("change",function() {
             if (model.ready())
                 popup.showAccept();
             else
                 popup.hideAccept();

             if (model.hasData()) {
               popup.setWidth(1018);
             } else {
               popup.setWidth(530);
             }
             popup.fixOverlay();
        });
};

