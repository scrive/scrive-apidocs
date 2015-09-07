define(["legacy_code"], function (legacy_code) {
  var clearTypeSetters = function (field) {
      _.each(field.signatory().document().signatories(), function (s) {
        _.each(s.fields(), function (f) {
          _.each(f.placements(), function (p) {
              if (p.typeSetter && p.withTypeSetter()) {
                  p.typeSetter.clear();
              }
         });
       });
     });
  };

  var boundInt = function (i, min, max) {
    if (i < min) { return min; }
    if (i > max) { return max; }
    return i;
  };

  var placementRect = function (field) {
    var placementWidth = 0;
    var placementHeight = 0;
    if (field.isText() || field.isFake()) {
      placementWidth = FieldPlacementGlobal.textfieldWidth;
      placementHeight = FieldPlacementGlobal.textfieldHeight;
    } else if (field.isCheckbox()) {
      placementWidth = FieldPlacementGlobal.checkboxWidth;
      placementHeight = FieldPlacementGlobal.checkboxHeight;
    } else if (field.isSignature()) {
      placementWidth = FieldPlacementGlobal.signatureWidth;
      placementHeight = FieldPlacementGlobal.signatureHeight;
    }

    return {
      width: placementWidth,
      height: placementHeight
    };
  };

  var stacking = function (type, bounds, left, top, step) {
    var x = 0;
    var y = 0;

    var $hit = $(document.elementFromPoint(left, top));
    var $temp = null;

    // stack only placements of the same type
    while ($hit.closest(".js-" + type).length !== 0) {
      x = boundInt(x + step, 0, bounds.right - left - step);
      y = boundInt(y + step, 0, bounds.bottom - top - step);
      $temp = $(document.elementFromPoint(left + x, top + y));
      if ($temp[0] === $hit[0]) { break; }
      $hit = $temp;
    }

    return {x: x, y: y};
  };

  var placeOnDocument = function ($handler, field, onDrop) {
    var sig = field.signatory();
    var doc = sig.document();

    var margin = 10;
    var offsetY = 50;
    var offsetX = Math.floor($handler.width() / 2);

    var viewportHandler = $handler[0].getBoundingClientRect();

    var left = viewportHandler.left + offsetX;
    var top = viewportHandler.bottom + offsetY;

    clearTypeSetters(field);

    var $hit = $(document.elementFromPoint(left, top));

    // in case we hit the margin between pages.
    if ($hit.closest(".pagediv").length === 0) {
      offsetY += 50;
      $hit = $(document.elementFromPoint(left, top));
    }

    var $page = $($hit.closest(".pagediv")[0]);

    // page could not be found, abort.
    if (!$page) { return ; }

    var viewportPage = $page[0].getBoundingClientRect();

    // stacking
    var stackingOffset = stacking(field.type(), viewportPage, left, top, margin);
    offsetX += stackingOffset.x;
    offsetY += stackingOffset.y;

    // center placement.
    var placementSize = placementRect(field);

    offsetX -= Math.floor(placementSize.width / 2);

    // make sure we hit a placement if it's there by moving it 1 pixel.
    var elY = viewportHandler.bottom + offsetY - 1;
    var elX = viewportHandler.left + offsetX - 1;

    var x = elX - viewportPage.left;
    var y = elY - viewportPage.top;
    var w = $page.width();
    var h = $page.height();

    var page = doc.mainfile().page($page.data("id"));

    x = boundInt(x, 0, w - (placementSize.width + margin));
    y = boundInt(y, 0, h - (placementSize.height + margin));

    onDrop(page, x, y, w, h);
  };

  return placeOnDocument;
});
