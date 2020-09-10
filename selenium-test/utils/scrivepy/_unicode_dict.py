import type_value_unifier as tvu
from scrivepy import _object


class TextMappingOrIterable(tvu.TypeValueUnifier):

    def type_check(self):
        value = self._value
        if isinstance(value, dict):
            return
        try:
            iter(value)
            return
        except TypeError:
            err_msg = ('%s must be a mapping or iterable, not %s'
                       % (self._variable_name, str(self._value)))
            raise TypeError(err_msg)

    def unify(self, value):
        if isinstance(value, dict):
            value = value.items()
        result = {}
        for pair in iter(value):
            try:
                key, val = pair
            except Exception:
                self.error('')
            else:
                key = \
                    tvu.Text(self._variable_name + ' key').unify_validate(key)
                val = tvu.Text(self._variable_name + ' value') \
                         .unify_validate(val)
                result[key] = val
        return result


class UnicodeDict(dict, _object.ScriveObject):

    @tvu.validate_and_unify(iterable=TextMappingOrIterable,
                            kwargs=TextMappingOrIterable)
    def __init__(self, iterable=(), **kwargs):
        dict.__init__(self, iterable, **kwargs)
        _object.ScriveObject.__init__(self)
        self._derived_objs = []

    def _set_read_only(self):
        for obj in self._derived_objs:
            obj._set_read_only()
        super(UnicodeDict, self)._set_read_only()

    def _set_invalid(self):
        for obj in self._derived_objs:
            obj._set_invalid()
        super(UnicodeDict, self)._set_invalid()

    def clear(self):
        self._check_setter()
        return dict.clear(self)

    def copy(self):
        self._check_getter()
        result = UnicodeDict(self)
        self._derived_objs.append(result)
        if self._read_only:
            result._set_read_only()
        return result

    @classmethod
    @tvu.validate_and_unify(keys=tvu.Iterable, value=tvu.Text)
    def fromkeys(cls, keys, value=''):
        keys = [tvu.Text('keys[%d]' % (i,)).unify_validate(key)
                for i, key in enumerate(keys)]
        return UnicodeDict([(key, value) for key in keys])

    def get(self, key, default=None):
        self._check_getter()
        return dict.get(self, key, default)

    def __contains__(self, key):
        self._check_getter()
        return dict.__contains__(self, key)

    def items(self):
        self._check_getter()
        return dict.items(self)

    def iteritems(self):
        self._check_getter()
        return dict.iteritems(self)

    def iterkeys(self):
        self._check_getter()
        return dict.iterkeys(self)

    def itervalues(self):
        self._check_getter()
        return dict.itervalues(self)

    def keys(self):
        self._check_getter()
        return dict.keys(self)

    def values(self):
        self._check_getter()
        return dict.values(self)

    def viewkeys(self):
        self._check_getter()
        return dict.viewkeys(self)

    def viewvalues(self):
        self._check_getter()
        return dict.viewvalues(self)

    def viewitems(self):
        self._check_getter()
        return dict.viewitems(self)

    def pop(self, key, *default):
        self._check_setter()
        return dict.pop(self, key, *default)

    def popitem(self):
        self._check_setter()
        return dict.popitem(self)

    @tvu.validate_and_unify(default=tvu.Text)
    def setdefault(self, key, default=''):
        self._check_setter()
        return dict.setdefault(self, key, default)

    @tvu.validate_and_unify(iterable=TextMappingOrIterable,
                            kwargs=TextMappingOrIterable)
    def update(self, iterable=(), **kwargs):
        self._check_setter()
        return dict.update(self, iterable, **kwargs)

    def __delitem__(self, key):
        self._check_setter()
        return dict.__delitem__(self, key)

    @tvu.validate_and_unify(other=tvu.instance(dict))
    def __eq__(self, other):
        self._check_getter()

        if not isinstance(other, UnicodeDict):
            return False

        return self._read_only == other._read_only and dict.__eq__(self, other)

    @tvu.validate_and_unify(other=tvu.instance(dict))
    def __ne__(self, other):
        self._check_getter()

        if not isinstance(other, UnicodeDict):
            return True

        return self._read_only != other._read_only or dict.__ne__(self, other)

    @tvu.validate_and_unify(other=tvu.instance(dict))
    def __ge__(self, other):
        self._check_getter()
        return dict.__ge__(self, other)

    @tvu.validate_and_unify(other=tvu.instance(dict))
    def __le__(self, other):
        self._check_getter()
        return dict.__le__(self, other)

    @tvu.validate_and_unify(other=tvu.instance(dict))
    def __gt__(self, other):
        self._check_getter()
        return dict.__gt__(self, other)

    @tvu.validate_and_unify(other=tvu.instance(dict))
    def __lt__(self, other):
        self._check_getter()
        return dict.__lt__(self, other)

    def __getitem__(self, key):
        self._check_getter()
        return dict.__getitem__(self, key)

    @tvu.validate_and_unify(key=tvu.Text, value=tvu.Text)
    def __setitem__(self, key, value):
        self._check_setter()
        return dict.__setitem__(self, key, value)

    def __len__(self):
        self._check_getter()
        return dict.__len__(self)

    def __iter__(self):
        self._check_getter()
        return dict.__iter__(self)
