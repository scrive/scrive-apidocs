import $ from "jquery";

const PIXEL_SPEED = 800;
const MAX_SCROLL_TIME = 2000;
const SCROLL_MARGIN = 150;

function get$top ($el) {
  return $("html,body");
}

function get$left ($el) {
  return $el.hasClass("placedfield") ? $(".scroller") : $(window);
}

function scrollWithInterrupt ($el, scrollKey, scrollValue, timeToScroll, finish) {
  finish = finish || function () { };
  const $all = $("html,body");

  function stop () {
    $el.stop(true, false);

    $all.unbind("mousewheel", stop);
    $all.unbind("mousedown", stop);
    $all.unbind("keydown", stop);
    $all.unbind("touchstart", stop);

    finish();
  }

  const scroll = {};
  scroll[scrollKey] = scrollValue;

  $el.animate(scroll, timeToScroll, finish);

  $all.bind("mousewheel", stop);
  $all.bind("mousedown", stop);
  $all.bind("keydown", stop);
  $all.bind("touchstart", stop);
}

function computeTargetScrollTop ($el) {
  const viewportHeight = window.innerHeight || $(window).height();
  const documentHeight = $(document).height();
  const elementOffsetTop = $el.offset().top;
  const elementHeight = $el.outerHeight();
  const currentScrollTop = $(window).scrollTop();

  let targetScrollTop = elementOffsetTop - SCROLL_MARGIN;
  if (currentScrollTop < elementOffsetTop) {
    const marginTop = 50;
    const elementWithBottom = documentHeight - elementOffsetTop;
    const fitsWithBottom = elementWithBottom + marginTop < viewportHeight;

    if (fitsWithBottom) {
      targetScrollTop = documentHeight - viewportHeight;
    } else {
      let scrollBottom = elementOffsetTop + elementHeight + SCROLL_MARGIN;
      targetScrollTop = scrollBottom - viewportHeight;
    }
  }

  if (targetScrollTop > elementOffsetTop) {
    targetScrollTop = elementOffsetTop;
  }

  return targetScrollTop;
}

function computeTargetScrollLeft ($el) {
  const viewportWidth = window.innerWidth || $(window).width();
  const elementOffsetLeft = $el.position().left;
  const elementWidth = $el.outerWidth();
  const elementCenter = elementOffsetLeft + (elementWidth / 2);
  const currentScrollLeft = get$left($el).scrollLeft();
  const scrollCenter = currentScrollLeft + (viewportWidth / 2);
  const scrollDiff = elementCenter - scrollCenter;

  return currentScrollLeft + scrollDiff;
}

function scrollToElement ($el, cb) {
  cb = cb || function () { };

  if ($el instanceof HTMLElement) {
    $el = $($el);
  }

  const currentScrollTop = $(window).scrollTop();
  const currentScrollLeft = get$left($el).scrollLeft();

  const targetScrollTop = computeTargetScrollTop($el);
  const targetScrollLeft = computeTargetScrollLeft($el);

  const distanceToTask = Math.abs(currentScrollTop - targetScrollTop) + Math.abs(currentScrollLeft - targetScrollLeft);

  let timeToScroll = Math.floor((distanceToTask / PIXEL_SPEED) * 1000);
  if (timeToScroll > MAX_SCROLL_TIME) {
    timeToScroll = MAX_SCROLL_TIME;
  }

  scrollWithInterrupt(get$top($el), "scrollTop", targetScrollTop, timeToScroll, cb);
  scrollWithInterrupt(get$left($el), "scrollLeft", targetScrollLeft, timeToScroll);

  return false;
}

module.exports = scrollToElement;
