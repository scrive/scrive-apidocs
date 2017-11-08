var CSVProblemModel = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/csvproblemmodel.jsx"
);

describe("designview/participants/csvsignatorydesignmodal/csvproblemmodel", function () {
  var model = null;

  beforeEach(function () {
    model = new CSVProblemModel({
      row: 0,
      cell: 0,
      header: false,
      description: "spam"
    });
  });

  it("should initialize with defaults", function () {
    model = new CSVProblemModel();
    assert.isUndefined(model.row());
    assert.isUndefined(model.cell());
    assert.isFalse(model.header());
    assert.notEqual(model.description());
  });

  it("should access the row index", function () {
    assert.equal(model.row(), 0);
  });

  it("should access the row index", function () {
    assert.equal(model.cell(), 0);
  });

  it("should access the header flag", function () {
    assert.isFalse(model.header());
  });

  describe("aboutCell", function () {
    it("should return false if row index doesn't match", function () {
      var result = model.aboutCell(1, 0);
      assert.isFalse(result);
    });

    it("should return false if cell index doesn't match", function () {
      var result = model.aboutCell(0, 1);
      assert.isFalse(result);
    });

    it("should return true if row and cell indexes match", function () {
      var result = model.aboutCell(0, 0);
      assert.isTrue(result);
    });
  });

  describe("aboutRow", function () {
    it("should return false if row index doesn't match", function () {
      model.set({cell: undefined});

      var result = model.aboutRow(1);
      assert.isFalse(result);
    });

    it("should return false if cell index isn't undefined", function () {
      var result = model.aboutRow(0);
      assert.isFalse(result);
    });

    it("should return true if row index matches and cell index is undefined", function () {
      model.set({cell: undefined});

      var result = model.aboutRow(0);
      assert.isTrue(result);
    });
  });

  it("should access the description", function () {
    assert.equal(model.description(), "spam");
  });

  describe("generalProblem", function () {
    it("should return false if row index isn't undefined", function () {
      model.set({cell: undefined});

      assert.isFalse(model.generalProblem());
    });

    it("should return false if cell index isn't undefined", function () {
      model.set({row: undefined});

      assert.isFalse(model.generalProblem());
    });

    it("should return true if row and cell indexes are undefined", function () {
      model.set({row: undefined, cell: undefined});

      assert.isTrue(model.generalProblem());
    });
  });
});
