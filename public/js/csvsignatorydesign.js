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
      rows : [],
      problems : []
  },
  problems : function() {
     return this.get("problems");
  },
  ready : function() {
           return (this.rows().length > 0) &&  (this.problems().length  == 0);
  },
  rows : function() {
     return this.get("rows");
  },
  maxRowLength : function() {
    var max = 0;
    _.each(this.rows(),function(r){
         max = (max > r.length) ? max : r.length;
    });
    return max;
  },
  isEmpty : function() {
     return (this.rows().length == 0) &&  (this.problems().length  == 0);
  },
  signatory: function() {
      return this.get("signatory");
  },
  initialize: function (args) {
      if (args.signatory.isCsv())
          this.set({"rows" : args.signatory.csv()});
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
  upload : function(input) {
       var sigdesign = this;
       var submit = new Submit({ url : "/parsecsv", method : "POST", expectedType:"json"});
       submit.add("customfieldscount",this.signatory().customFields().length);
       if (this.signatory().document().elegAuthentication())
        submit.add("eleg","YES");
       submit.addInputs(input);
       submit.sendAjax(function (resp) {
           var jresp = resp;
           var extraproblems = [];
           if(designViewBlocking && designViewBlocking.shouldBlockDocs(jresp.rows.length)) {
               extraproblems.push(new CsvProblem({description:designViewBlocking.csvMessage()}));
           }

           var problems = _.map(jresp.problems, function(pdata) {return new CsvProblem(pdata);});
           problems = problems.concat(extraproblems);
           sigdesign.set({'rows': jresp.rows, 'problems': problems });
           sigdesign.trigger("change");
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
    // we fix the order of the standard fields
    csvheaderorder: ['fstname', 'sndname', 'email', 'sigco', 'sigpersnr', 'sigcompnr'],
    dataTable : function() {
      var model = this.model;
      var table = $("<table class='csvDataTable'/>");
      var thead = $("<thead/>");
      var tbody = $("<tbody/>");
      table.append(thead).append(tbody);

      // make sure the field names are in the right order
      var fieldnames = 
            this.csvheaderorder.concat(
                _.difference(_.invoke(model.signatory().fields(),  'name'),
                             this.csvheaderorder));
      _.each(fieldnames, function(e) {
          var field = model.signatory().field(e,"standard");
          if (field == undefined)
              field =  model.signatory().field(e,"custom");
          if (field != undefined)
              thead.append($("<th />").text(field.nicename()));
      });

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
           td.append($("<div class='problem'>").text(model.problemWithRow().description()));
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
                    color : "blue",
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


window.CsvSignatoryDesignPopup = {
    popup: function(args) {
         var signatory = args.signatory;
         var model = new CsvSignatoryDesign({ signatory : signatory  });
         var view = new CsvSignatoryDesignView({model : model, el : $("<div/>")});
         var popup = Confirmation.popup({
              content  : $(view.el),
              title  : localization.csv.title,
              acceptText: localization.save,
              width: "960px",
              acceptVisible : model.ready(),
              onAccept : function() {
                  signatory.makeCsv(model.rows());
                  signatory.document().save();
                  return true;
            }
        });
        model.bind("change",function() {
             if (model.ready())
                 popup.showAccept();
             else
                 popup.hideAccept(); });

    }
};


})(window);
