import enum

import type_value_unifier as tvu
from scrivepy import _object, _set, _field_placement, _exceptions


scrive_property = _object.scrive_property
ScriveSet = _set.ScriveSet


class Field(_object.ScriveObject):

    _default_placement_tip = _field_placement.TipSide.right_tip

    @tvu.validate_and_unify(value=tvu.instance(unicode),
                            obligatory=tvu.instance(bool),
                            should_be_filled_by_sender=tvu.instance(bool))
    def __init__(self, value=u'', obligatory=True,
                 should_be_filled_by_sender=False):
        super(Field, self).__init__()
        self._value = value
        self._closed = None
        self._obligatory = obligatory
        self._should_be_filled_by_sender = should_be_filled_by_sender
        self._placements = ScriveSet()
        self._placements._elem_validator = \
            tvu.instance(_field_placement.FieldPlacement)

    def __str__(self):
        return u'%s(value=%s, %d placements)' % \
            (unicode(self.__class__.__name__),
             self.value, len(self.placements()))

    @classmethod
    def _from_json_obj(cls, json):
        try:
            type_ = json[u'type']
            name = json[u'name']
            value = json[u'value']
            closed = json.get(u'closed')
            obligatory = json[u'obligatory']
            should_be_filled_by_sender = json[u'shouldbefilledbysender']
            placements = \
                [_field_placement.FieldPlacement._from_json_obj(
                    placement_json) for placement_json in json[u'placements']]

            if type_ == u'standard':
                field = StandardField(name=StandardFieldType(name),
                                      value=value)
            elif type_ == u'custom':
                field = CustomField(name=name, value=value)
            elif type_ == u'signature':
                field = SignatureField(name=name)
                if not isinstance(value, unicode):
                    raise _exceptions.InvalidResponse(u'bad field value')
                field._value = value
            elif type_ == u'checkbox':
                field = CheckboxField(name=name,
                                      value=value.lower() == u'checked')
            else:
                raise _exceptions.InvalidResponse(u'bad field type')

            field.obligatory = obligatory
            field.should_be_filled_by_sender = should_be_filled_by_sender
            field.placements.update(placements)
            if isinstance(closed, (bool, type(None))):
                field._closed = closed
            else:
                raise _exceptions.InvalidResponse()
            return field
        except (KeyError, TypeError, ValueError) as e:
            raise _exceptions.InvalidResponse(e)

    def _set_invalid(self):
        # invalidate placements first, before getter stops working
        self.placements._set_invalid()
        super(Field, self)._set_invalid()

    def _set_read_only(self):
        super(Field, self)._set_read_only()
        self.placements._set_read_only()

    def _to_json_obj(self):
        for placement in self.placements:
            placement._resolve_default_tip(self._default_placement_tip)
        return {u'name': self.name,
                u'obligatory': self.obligatory,
                u'shouldbefilledbysender': self.should_be_filled_by_sender,
                u'type': self.type,
                # checkbox stores different type than getter returns
                u'value': self._value,
                u'placements': list(self.placements)}

    @scrive_property
    def type(self):
        return self._type

    @scrive_property
    def name(self):
        return self._name

    @scrive_property
    def value(self):
        return self._value

    @value.setter
    @tvu.validate_and_unify(value=tvu.instance(unicode))
    def value(self, value):
        self._value = value

    @scrive_property
    def closed(self):
        return self._closed

    @scrive_property
    def obligatory(self):
        return self._obligatory

    @obligatory.setter
    @tvu.validate_and_unify(obligatory=tvu.instance(bool))
    def obligatory(self, obligatory):
        self._obligatory = obligatory

    @scrive_property
    def should_be_filled_by_sender(self):
        return self._should_be_filled_by_sender

    @should_be_filled_by_sender.setter
    @tvu.validate_and_unify(should_be_filled_by_sender=tvu.instance(bool))
    def should_be_filled_by_sender(self, should_be_filled_by_sender):
        self._should_be_filled_by_sender = should_be_filled_by_sender

    @scrive_property
    def placements(self):
        return self._placements


class StandardFieldType(unicode, enum.Enum):
    first_name = u'fstname'
    last_name = u'sndname'
    email = u'email'
    mobile = u'mobile'
    personal_number = u'sigpersnr'
    company_name = u'sigco'
    company_number = u'sigcompnr'


class StandardField(Field):

    @tvu.validate_and_unify(name=tvu.instance(StandardFieldType, enum=True),
                            value=tvu.instance(unicode),
                            obligatory=tvu.instance(bool),
                            should_be_filled_by_sender=tvu.instance(bool))
    def __init__(self, name, value=u'', obligatory=True,
                 should_be_filled_by_sender=False):
        super(StandardField, self).__init__(
            value=value, obligatory=obligatory,
            should_be_filled_by_sender=should_be_filled_by_sender)
        self._type = u'standard'
        self._name = name


class CustomField(Field):

    @tvu.validate_and_unify(name=tvu.instance(unicode),
                            value=tvu.instance(unicode),
                            obligatory=tvu.instance(bool),
                            should_be_filled_by_sender=tvu.instance(bool))
    def __init__(self, name, value=u'', obligatory=True,
                 should_be_filled_by_sender=False):
        super(CustomField, self).__init__(
            value=value, obligatory=obligatory,
            should_be_filled_by_sender=should_be_filled_by_sender)
        self._type = u'custom'
        self._name = name

    @scrive_property
    def name(self):
        return self._name

    @name.setter
    @tvu.validate_and_unify(name=tvu.instance(unicode))
    def name(self, name):
        self._name = name


class SignatureField(Field):

    @tvu.validate_and_unify(name=tvu.instance(unicode),
                            obligatory=tvu.instance(bool),
                            should_be_filled_by_sender=tvu.instance(bool))
    def __init__(self, name, obligatory=True,
                 should_be_filled_by_sender=False):
        super(SignatureField, self).__init__(
            value=u'', obligatory=obligatory,
            should_be_filled_by_sender=should_be_filled_by_sender)
        self._type = u'signature'
        self._name = name

    @scrive_property
    def name(self):
        return self._name

    @name.setter
    @tvu.validate_and_unify(name=tvu.instance(unicode))
    def name(self, name):
        self._name = name

    @scrive_property
    def value(self):
        return self._value


class CheckboxField(Field):

    _default_placement_tip = _field_placement.TipSide.left_tip

    @tvu.validate_and_unify(name=tvu.instance(unicode),
                            value=tvu.instance(bool),
                            obligatory=tvu.instance(bool),
                            should_be_filled_by_sender=tvu.instance(bool))
    def __init__(self, name, value=False, obligatory=False,
                 should_be_filled_by_sender=False):
        super(CheckboxField, self).__init__(
            value=u'CHECKED' if value else u'', obligatory=obligatory,
            should_be_filled_by_sender=should_be_filled_by_sender)
        self._type = u'checkbox'
        self._name = name

    @scrive_property
    def name(self):
        return self._name

    @name.setter
    @tvu.validate_and_unify(name=tvu.instance(unicode))
    def name(self, name):
        self._name = name

    @scrive_property
    def value(self):
        return self._value.lower() == u'checked'

    @value.setter
    @tvu.validate_and_unify(value=tvu.instance(bool))
    def value(self, value):
        self._value = u'CHECKED' if value else u''
