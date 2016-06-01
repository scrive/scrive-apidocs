import enum

import type_value_unifier as tvu
from scrivepy import _object

scrive_property = _object.scrive_property


class Ratio(tvu.TypeValueUnifier):

    TYPES = (float, int)

    def unify(self, value):
        return float(value)

    def validate(self, value):
        if not 0. <= value <= 1.:
            self.error(u'in the <0,1> range (inclusive)')


class TipSide(unicode, enum.Enum):
    left_tip = u'left'
    right_tip = u'right'


class MaybeTipSide(tvu.EnumTypeValueUnifier):

    TYPES = (TipSide, type(None))


class FieldPlacement(_object.ScriveObject):

    FONT_SIZE_SMALL = 12. / 943.
    FONT_SIZE_NORMAL = 16. / 943.
    FONT_SIZE_LARGE = 20. / 943.
    FONT_SIZE_HUGE = 24. / 943.

    @tvu.validate_and_unify(left=Ratio, top=Ratio, width=Ratio,
                            height=Ratio, font_size=Ratio,
                            page=tvu.PositiveInt, tip=MaybeTipSide)
    def __init__(self, left, top, width, height,
                 font_size=FONT_SIZE_NORMAL, page=1, tip=None):
        super(FieldPlacement, self).__init__()
        self._left = left
        self._top = top
        self._width = width
        self._height = height
        self._font_size = font_size
        self._page = page
        self._tip = tip

    def _to_json_obj(self):
        return {u'xrel': self.left,
                u'yrel': self.top,
                u'wrel': self.width,
                u'hrel': self.height,
                u'fsrel': self.font_size,
                u'page': self.page,
                u'tip': self.tip.value if self.tip else None}

    def __str__(self):
        return u'Placement(page ' + str(self.page) + u',' + \
            str(self.left) + u':' + str(self.top) + u')'

    @classmethod
    def _from_json_obj(cls, json):
        return FieldPlacement(left=json[u'xrel'], top=json[u'yrel'],
                              width=json[u'wrel'], height=json[u'hrel'],
                              font_size=json[u'fsrel'], page=json[u'page'],
                              tip=TipSide(json[u'tip']))

    def _resolve_default_tip(self, default_tip_value):
        self._check_invalid()
        if self.tip is None:
            self.tip = default_tip_value

    @scrive_property
    def left(self):
        return self._left

    @left.setter
    @tvu.validate_and_unify(left=Ratio)
    def left(self, left):
        self._left = left

    @scrive_property
    def top(self):
        return self._top

    @top.setter
    @tvu.validate_and_unify(top=Ratio)
    def top(self, top):
        self._top = top

    @scrive_property
    def width(self):
        return self._width

    @width.setter
    @tvu.validate_and_unify(width=Ratio)
    def width(self, width):
        self._width = width

    @scrive_property
    def height(self):
        return self._height

    @height.setter
    @tvu.validate_and_unify(height=Ratio)
    def height(self, height):
        self._height = height

    @scrive_property
    def font_size(self):
        return self._font_size

    @font_size.setter
    @tvu.validate_and_unify(font_size=Ratio)
    def font_size(self, font_size):
        self._font_size = font_size

    @scrive_property
    def page(self):
        return self._page

    @page.setter
    @tvu.validate_and_unify(page=tvu.PositiveInt)
    def page(self, page):
        self._page = page

    @scrive_property
    def tip(self):
        tip = self._tip
        if tip is not None:
            tip = TipSide(tip)
        return tip

    @tip.setter
    @tvu.validate_and_unify(tip=MaybeTipSide)
    def tip(self, tip):
        self._tip = tip
