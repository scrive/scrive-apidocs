/*Popup when someone want to select a csv signatory. At the end it can set signatory csv.
 */

(function(window){

var CsvProblem = Backbone.Model.extend({
      defaults : {
      row : undefined,
      cell : undefined,
      description : "Some problem"
  },
  row: function() {
     return this.get("row");
  },
  cell: function() {
     return this.get("cell");
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
           return (this.rows().length > 0) &&  (this.problems().length  == 0);
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
  csvstandardheaders : ['fstname', 'sndname', 'email', 'mobile',  'sigco', 'sigpersnr', 'sigcompnr'],
  upload : function(input) {
       var self = this;
       var submit = new Submit({ url : "/parsecsv", method : "POST", expectedType:"text"});
       submit.addInputs(input);
       submit.sendAjax(function (resp) {
           var jresp = JSON.parse(resp);
           var problems = [];
           if (jresp.parseError)
              problems.push(new CsvProblem({description: "Parse problem"}));
           if(BlockingInfo && jresp.rows && BlockingInfo.shouldBlockDocs(jresp.rows.length)) {
               problems.push(new CsvProblem({description:BlockingInfo.csvMessage(jresp.rows.length)}));
           }
           if (jresp.header != undefined) {
              for(var i=0;i<jresp.header.length;i++)
                jresp.header[i] = self.csvstandardheaders[i] || jresp.header[i];
           }
           self.set({'header': jresp.header , 'rows': jresp.rows, 'problems': problems });
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
          box.append($("<div class='problem'>").html(p.description()));
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
                return localization.personamNumber;
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
    firstUploadBox : function(){
         var model = this.model;
         var box = $("<div class='option-box middle-box'>");
         var header = $("<div class='header'/>").text(localization.csv.selectFileHeader);
         var subheader = $("<div class='sheader'/>").text(localization.csv.selectFileSubheader);
         var uploadButton = UploadButton.init({
                    name: "csv",
                    color : "green",
                    size: "small",
                    text: localization.csv.selectFile,
                    width: 200,
                    type: "application/csv",
                    onAppend : function(input) {
                        setTimeout(function() {model.upload($(input));},100);

                    },
                    onError : function() {}
            });
         box.append(header);
         box.append(subheader);
         box.append($("<div class='buttonbox'/>").append(uploadButton.input()));
         return box;
    },
    nextUploadButton : function(){
         var model = this.model;
         var uploadButton = UploadButton.init({
                    name: "csv",
                    color : "green",
                    size: "tiny",
                    text: localization.csv.selectOtherFile,
                    width: 100,
                    type: "application/csv",
                    onAppend : function(input) {
                        setTimeout(function() {model.upload($(input));},100);

                    },
                    onError : function() {}
            });
         return uploadButton.input();
    },
    render: function () {
        var view = this;
        var model = this.model;
        this.container = $(this.el);
        this.container.addClass("designCSVSignatoryPopupContent");
        this.container.empty();
        if (model.isEmpty()) {
            this.container.append(this.firstUploadBox());
        }
        else {
            this.container.append(this.generalProblemsList());
            this.container.append(this.dataTable());
            this.container.append(this.nextUploadButton());
        }
        return this;
    }
});


window.CsvSignatoryDesignPopup =  function(args) {
         var csv = [];
         var csvSignatory = undefined;
         var document = args.document;
         _.each(document.signatories(), function(s) {
           if (s.isCsv()) {
             csv = s.csv();
             csvSignatory = s
           };
         })

         var model = new CsvSignatoryDesign({ header: _.first(csv),  rows : _.rest(csv)  });
         var view = new CsvSignatoryDesignView({model : model, el : $("<div/>")});
         var popup = Confirmation.popup({
              content  : $(view.el),
              title  : localization.csv.title,
              acceptText: localization.save,
              width: 960,
              acceptVisible : model.ready(),
              onAccept : function() {
                  if (csvSignatory == undefined) {
                    var fields = [];
                    for(var i = 0; i < model.header().length; i ++)
                       fields.push({name: model.header()[i],   type : _.contains(model.csvstandardheaders,model.header()[i]) ? "standard" : "custom"});
                       var signatory = new Signatory({
                         document : document,
                         fields : fields,
                         csv : model.csv()
                      });
                    document.addExistingSignatory(signatory);
                  } else {
                    for(var i = 0; i < model.header().length; i ++)
                      if (!csvSignatory.hasTextFieldWithName(model.header()[i])) {
                        csvSignatory.addField(new Field({
                              name: model.header()[i],
                              type: _.contains(model.csvstandardheaders,model.header()[i]) ? "standard" : "custom",
                              signatory: csvSignatory,
                          }));
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
                 popup.hideAccept(); });
};


})(window);
