import enum

import type_value_unifier as tvu
from scrivepy import _object, _set, _field_placement, _exceptions


scrive_property = _object.scrive_property
ScriveSet = _set.ScriveSet


class Field(_object.ScriveObject):

    _default_placement_tip = _field_placement.TipSide.right_tip

    @tvu.validate_and_unify(value=tvu.instance(str),
                            obligatory=tvu.instance(bool),
                            should_be_filled_by_sender=tvu.instance(bool))
    def __init__(self, value='', obligatory=True,
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
        return '%s(value=%s, %d placements)' % \
            (str(self.__class__.__name__),
             self.value, len(self.placements()))

    @classmethod
    def _from_json_obj(cls, json):
        try:
            type_ = json['type']
            name = json['name']
            value = json['value']
            closed = json.get('closed')
            obligatory = json['obligatory']
            should_be_filled_by_sender = json['shouldbefilledbysender']
            placements = \
                [_field_placement.FieldPlacement._from_json_obj(
                    placement_json) for placement_json in json['placements']]

            if type_ == 'standard':
                field = StandardField(name=StandardFieldType(name),
                                      value=value)
            elif type_ == 'custom':
                field = CustomField(name=name, value=value)
            elif type_ == 'signature':
                field = SignatureField(name=name)
                if not isinstance(value, str):
                    raise _exceptions.InvalidResponse('bad field value')
                field._value = value
            elif type_ == 'checkbox':
                field = CheckboxField(name=name,
                                      value=value.lower() == 'checked')
            else:
                raise _exceptions.InvalidResponse('bad field type')

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
        return {'name': self.name,
                'obligatory': self.obligatory,
                'shouldbefilledbysender': self.should_be_filled_by_sender,
                'type': self.type,
                # checkbox stores different type than getter returns
                'value': self._value,
                'placements': list(self.placements)}

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
    @tvu.validate_and_unify(value=tvu.instance(str))
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


class StandardFieldType(str, enum.Enum):
    first_name = 'fstname'
    last_name = 'sndname'
    email = 'email'
    mobile = 'mobile'
    personal_number = 'sigpersnr'
    company_name = 'sigco'
    company_number = 'sigcompnr'


class StandardField(Field):

    @tvu.validate_and_unify(name=tvu.instance(StandardFieldType, enum=True),
                            value=tvu.instance(str),
                            obligatory=tvu.instance(bool),
                            should_be_filled_by_sender=tvu.instance(bool))
    def __init__(self, name, value='', obligatory=True,
                 should_be_filled_by_sender=False):
        super(StandardField, self).__init__(
            value=value, obligatory=obligatory,
            should_be_filled_by_sender=should_be_filled_by_sender)
        self._type = 'standard'
        self._name = name


class CustomField(Field):

    @tvu.validate_and_unify(name=tvu.instance(str),
                            value=tvu.instance(str),
                            obligatory=tvu.instance(bool),
                            should_be_filled_by_sender=tvu.instance(bool))
    def __init__(self, name, value='', obligatory=True,
                 should_be_filled_by_sender=False):
        super(CustomField, self).__init__(
            value=value, obligatory=obligatory,
            should_be_filled_by_sender=should_be_filled_by_sender)
        self._type = 'custom'
        self._name = name

    @scrive_property
    def name(self):
        return self._name

    @name.setter
    @tvu.validate_and_unify(name=tvu.instance(str))
    def name(self, name):
        self._name = name


class SignatureField(Field):

    @tvu.validate_and_unify(name=tvu.instance(str),
                            obligatory=tvu.instance(bool),
                            should_be_filled_by_sender=tvu.instance(bool))
    def __init__(self, name, obligatory=True,
                 should_be_filled_by_sender=False):
        super(SignatureField, self).__init__(
            value='', obligatory=obligatory,
            should_be_filled_by_sender=should_be_filled_by_sender)
        self._type = 'signature'
        self._name = name

    @scrive_property
    def name(self):
        return self._name

    @name.setter
    @tvu.validate_and_unify(name=tvu.instance(str))
    def name(self, name):
        self._name = name

    @scrive_property
    def value(self):
        return self._value


class CheckboxField(Field):

    _default_placement_tip = _field_placement.TipSide.left_tip

    @tvu.validate_and_unify(name=tvu.instance(str),
                            value=tvu.instance(bool),
                            obligatory=tvu.instance(bool),
                            should_be_filled_by_sender=tvu.instance(bool))
    def __init__(self, name, value=False, obligatory=False,
                 should_be_filled_by_sender=False):
        super(CheckboxField, self).__init__(
            value='CHECKED' if value else '', obligatory=obligatory,
            should_be_filled_by_sender=should_be_filled_by_sender)
        self._type = 'checkbox'
        self._name = name

    @scrive_property
    def name(self):
        return self._name

    @name.setter
    @tvu.validate_and_unify(name=tvu.instance(str))
    def name(self, name):
        self._name = name

    @scrive_property
    def value(self):
        return self._value.lower() == 'checked'

    @value.setter
    @tvu.validate_and_unify(value=tvu.instance(bool))
    def value(self, value):
        self._value = 'CHECKED' if value else ''
