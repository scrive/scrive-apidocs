var underscore = require("underscore");

var CSVProblemModel = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/csvproblemmodel.jsx"
);
var CSVSignatoryDesignModel = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/csvsignatorydesignmodel.jsx"
);
var Submit = require("../../../../js/submits.js");

describe("designview/participants/csvsignatorydesignmodal/csvsignatorydesignmodel", function () {
  var fakeSubmit = null;
  var model = null;

  beforeEach(function () {
    fakeSubmit = new Submit.Submit();
    sinon.stub(fakeSubmit, "addInputs");
    sinon.stub(fakeSubmit, "sendAjax");
    sinon.stub(Submit, "Submit").returns(fakeSubmit);

    model = new CSVSignatoryDesignModel({
      header: ["fstname", "sndname", "email"],
      problems: [
        new CSVProblemModel({row: 0, cell: 0, description: "spam"}),
        new CSVProblemModel({row: 0, cell: undefined, description: "spam"}),
        new CSVProblemModel({
          row: undefined,
          cell: undefined,
          description: "spam"
        })
      ],
      rows: [
        ["Spam", "Eggs", "spam@eggs.com"]
      ]
    });
  });

  afterEach(function () {
    Submit.Submit.restore();
  });

  it("should initialize with defaults", function () {
    model = new CSVSignatoryDesignModel();
    assert.isUndefined(model.header());
    assert.isArray(model.problems());
    assert.lengthOf(model.problems(), 0);
    assert.isUndefined(model.rows())
  });

  it("should access the problems array", function () {
    assert.equal(model.problems(), model.get("problems"));
  });

  describe("ready", function () {
    it("should return false if rows value is undefined", function () {
      model.set({rows: undefined});

      assert.isFalse(model.ready());
    });

    it("should return false if rows array is empty", function () {
      model.set({rows: []});

      assert.isFalse(model.ready());
    });

    it("should return false if problems array isn't empty", function () {
      assert.isFalse(model.ready());
    });

    it("should return true", function () {
      model.set({problems: []});

      assert.isTrue(model.ready());
    });
  });

  it("should access the header array", function () {
    assert.equal(model.header(), model.get("header"));
  });

  it("should access the rows array", function () {
    assert.equal(model.rows(), model.get("rows"));
  });

  it("should concatenate the header and rows arrays to form the CSV", function () {
    var result = model.csv();
    assert.lengthOf(result, 2);
    assert.equal(result[0], model.header());
    assert.equal(result[1], model.rows()[0]);
  });

  it("should compute the max row length", function () {
    model.rows().push(["Eggs", "Spam"]);

    assert.equal(model.maxRowLength(), 3);
  });

  describe("isEmpty", function () {
    it("should return true if header and rows values are undefined and problems array is empty", function () {
      model.set({header: undefined, rows: undefined, problems: []});

      assert.isTrue(model.isEmpty());
    });

    it("should return false if only header value is undefined", function () {
      model.set({header: undefined});

      assert.isFalse(model.isEmpty());
    });

    it("should return false if only rows value is undefined", function () {
      model.set({header: undefined});

      assert.isFalse(model.isEmpty());
    });

    it("should return false if header and rows values are undefined and problems array isn't empty", function () {
      model.set({header: undefined, rows: undefined});

      assert.isFalse(model.isEmpty());
    });
  });

  describe("hasData", function () {
    it("should return false if header and rows values are undefined", function () {
      model.set({header: undefined, rows: undefined});

      assert.isFalse(model.hasData());
    });

    it("should return false if both header and rows values are empty", function () {
      model.set({header: [], rows: []});

      assert.isFalse(model.hasData());
    });

    it("should return true if only header value is undefined", function () {
      model.set({header: undefined});

      assert.isTrue(model.hasData());
    });

    it("should return true if only header value is empty", function () {
      model.set({header: []});

      assert.isTrue(model.hasData());
    });

    it("should return true if only rows value is undefined", function () {
      model.set({rows: undefined});

      assert.isTrue(model.hasData());
    });

    it("should return true if only rows value is empty", function () {
      model.set({rows: []});

      assert.isTrue(model.hasData());
    });
  });

  describe("problemWithRow", function () {
    it("should return undefined if there's no problem matching row index", function () {
      var result = model.problemWithRow(1);
      assert.isUndefined(result);
    });

    it("should return problem that matches the row index", function () {
      var result = model.problemWithRow(0);
      assert.equal(result, model.problems()[1]);
    });
  });

  describe("problemWithCell", function () {
    it("should return undefined if there's no problem matching row and cell indexes", function () {
      var result = model.problemWithCell(1, 0);
      assert.isUndefined(result);
    });

    it("should return undefined if there's no problem matching cell indexes", function () {
      var result = model.problemWithCell(0, 1);
      assert.isUndefined(result);
    });

    it("should return problem that matches the row and cell indexes", function () {
      var result = model.problemWithCell(0, 0);
      assert.equal(result, model.problems()[0]);
    });
  });

  it("should return the general problems", function () {
    var result = model.generalProblems();
    assert.lengthOf(result, 1);
    assert.equal(result[0], model.problems()[2]);
  });

  describe("onUpload", function () {
    var response = {
      header: ["fstname", "sndname", "email"],
      rows: [
        ["Spam", "Eggs", "spam@eggs.com"],
        ["Spam", "Eggs", "spam@eggs.com"],
        ["Spameggs", "Eggsspam", "spameggs@eggsspam.com"]
      ]
    };

    it("should set formating error general problem if the file failed to parse", function () {
      sinon.stub(model, "set");

      model.onUpload({parseError: true});
      assert.isTrue(model.set.called);

      var update = model.set.args[0][0];
      assert.isUndefined(update.header);
      assert.isArray(update.problems);
      assert.lengthOf(update.problems, 1);
      assert.isUndefined(update.rows);

      var problem = update.problems[0];
      assert.equal(problem.description(), localization.csv.formatError);
      assert.isTrue(problem.header());
    });

    it("should set missing header general problem if the file header is undefined", function () {
      sinon.stub(model, "set");

      model.onUpload(underscore.extendOwn({}, response, {header: undefined}));
      assert.isTrue(model.set.called);

      var update = model.set.args[0][0];
      assert.isUndefined(update.header);
      assert.isArray(update.problems);
      assert.lengthOf(update.problems, 1);

      var problem = update.problems[0];
      assert.equal(problem.description(), localization.csv.atLeast3Columns);
    });

    it("should set missing header general problem if the file header has less than 3 columns", function () {
      sinon.stub(model, "set");

      model.onUpload(underscore.extendOwn({}, response, {
        header: ["fstname", "sndname"]
      }));
      assert.isTrue(model.set.called);

      var update = model.set.args[0][0];
      assert.isArray(update.problems);
      assert.lengthOf(update.problems, 1);

      var problem = update.problems[0];
      assert.equal(problem.description(), localization.csv.atLeast3Columns);
    });

    it("should set missing parties general problem if the file has no rows", function () {
      sinon.stub(model, "set");

      model.onUpload(underscore.extendOwn({}, response, {rows: []}));
      assert.isTrue(model.set.called);

      var update = model.set.args[0][0];
      assert.isArray(update.problems);
      assert.lengthOf(update.problems, 1);

      var problem = update.problems[0];
      assert.equal(problem.description(), localization.csv.atLeast1Party);
    });

    it("should fill empty column names with default ones", function () {
      sinon.stub(model, "set");

      model.onUpload(underscore.extendOwn({}, response, {
        header: ["", "sndname", "email"]
      }));
      assert.isTrue(model.set.called);

      var update = model.set.args[0][0];
      assert.equal(update.header[0], _.keys(model.csvstandardheaders)[0]);
    });

    it("should set the parsed data", function () {
      model = new CSVSignatoryDesignModel();
      sinon.stub(model, "trigger");

      model.onUpload(response);
      assert.equal(model.header(), response.header);
      assert.equal(model.rows(), response.rows);
      assert.lengthOf(model.problems(), 0);
      assert.isTrue(model.trigger.calledWith("change"));
    });
  });

  it("should configure and send the file upload request", function () {
    model.upload("spam");
    assert.isTrue(Submit.Submit.calledWithNew());
    assert.isTrue(Submit.Submit.calledWith({
      url: "/parsecsv",
      method: "POST"
    }));
    assert.isTrue(fakeSubmit.addInputs.calledWith("spam"));
    assert.isTrue(fakeSubmit.sendAjax.calledWith(model.onUpload));
  });
});
