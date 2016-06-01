import itertools

import type_value_unifier as tvu
from scrivepy import _object


class ScriveSet(set, _object.ScriveObject):

    @tvu.validate_and_unify(iterable=tvu.Iterable)
    def __init__(self, iterable=()):
        set.__init__(self, iterable)
        _object.ScriveObject.__init__(self)
        self.__init_scrive_set__()

    def __init_scrive_set__(self):
        self._derived_objs = []
        self._elem_validator = None

    def add(self, elem):
        self._check_setter()
        if self._elem_validator is not None:
            elem = self._elem_validator('elem').unify_validate(elem)
        return set.add(self, elem)

    def copy(self):
        self._check_getter()
        result = set.copy(self)
        result.__init_scrive_set__()
        _object.ScriveObject.__init__(result)
        self._derived_objs.append(result)
        if self._read_only:
            result._set_read_only()
        return result

    @tvu.validate_and_unify(iterables=tvu.args_of(tvu.Iterable))
    def difference_update(self, *iterables):
        self._check_setter()
        return set.difference_update(self, *iterables)

    @tvu.validate_and_unify(args=tvu.args_of(tvu.Iterable))
    def intersection(self, *args):
        self._check_getter()
        result = set.intersection(self, *args)
        result.__init_scrive_set__()
        _object.ScriveObject.__init__(result)
        self._derived_objs.append(result)
        return result

    @tvu.validate_and_unify(iterable=tvu.Iterable)
    def isdisjoint(self, iterable):
        self._check_getter()
        return set.isdisjoint(self, iterable)

    @tvu.validate_and_unify(iterable=tvu.Iterable)
    def issuperset(self, iterable):
        self._check_getter()
        return set.issuperset(self, iterable)

    def remove(self, elem):
        self._check_setter()
        return set.remove(self, elem)

    @tvu.validate_and_unify(iterable=tvu.Iterable)
    def symmetric_difference(self, iterable):
        self._check_getter()
        result = set.symmetric_difference(self, iterable)
        result.__init_scrive_set__()
        _object.ScriveObject.__init__(result)
        self._derived_objs.append(result)
        return result

    @tvu.validate_and_unify(iterable=tvu.Iterable)
    def symmetric_difference_update(self, iterable):
        self._check_setter()
        if self._elem_validator is not None:
            validator = self._elem_validator
            iterable = \
                [validator('iterable[%s]' % (i,)).unify_validate(elem)
                 for i, elem in enumerate(iterable)]
        return set.symmetric_difference_update(self, iterable)

    @tvu.validate_and_unify(iterables=tvu.args_of(tvu.Iterable))
    def update(self, *iterables):
        self._check_setter()
        if self._elem_validator is not None:
            validator = self._elem_validator
            iterables = \
                [[validator('iterables[%s][%s]' % (i, j)).unify_validate(elem)
                  for j, elem in enumerate(iterable)]
                 for i, iterable in enumerate(iterables)]
        return set.update(self, *iterables)

    def clear(self):
        self._check_setter()
        return set.clear(self)

    @tvu.validate_and_unify(args=tvu.args_of(tvu.Iterable))
    def difference(self, *args):
        self._check_getter()
        result = set.difference(self, *args)
        result.__init_scrive_set__()
        _object.ScriveObject.__init__(result)
        self._derived_objs.append(result)
        return result

    def discard(self, elem):
        self._check_setter()
        return set.discard(self, elem)

    @tvu.validate_and_unify(args=tvu.args_of(tvu.Iterable))
    def intersection_update(self, *args):
        self._check_setter()
        return set.intersection_update(self, *args)

    @tvu.validate_and_unify(iterable=tvu.Iterable)
    def issubset(self, iterable):
        self._check_getter()
        return set.issubset(self, iterable)

    def pop(self):
        self._check_setter()
        return set.pop(self)

    @tvu.validate_and_unify(args=tvu.args_of(tvu.Iterable))
    def union(self, *args):
        self._check_getter()
        result = set.union(self, *args)
        result.__init_scrive_set__()
        _object.ScriveObject.__init__(result)
        self._derived_objs.append(result)
        return result

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __and__(self, other):
        self._check_getter()
        result = set.__and__(self, other)
        result.__init_scrive_set__()
        _object.ScriveObject.__init__(result)
        self._derived_objs.append(result)
        return result

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __xor__(self, other):
        self._check_getter()
        result = set.__xor__(self, other)
        result.__init_scrive_set__()
        _object.ScriveObject.__init__(result)
        self._derived_objs.append(result)
        return result

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __sub__(self, other):
        self._check_getter()
        result = set.__sub__(self, other)
        result.__init_scrive_set__()
        _object.ScriveObject.__init__(result)
        self._derived_objs.append(result)
        return result

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __or__(self, other):
        self._check_getter()
        result = set.__or__(self, other)
        result.__init_scrive_set__()
        _object.ScriveObject.__init__(result)
        self._derived_objs.append(result)
        return result

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __ge__(self, other):
        self._check_getter()
        return set.__ge__(self, other)

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __le__(self, other):
        self._check_getter()
        return set.__le__(self, other)

    # this redirects to __or__ anyway, so no need for tvu wrapper
    def __ior__(self, other):
        self._check_setter()
        if self._elem_validator is not None:
            validator = self._elem_validator
            other = \
                [validator('other[%s]' % (i,)).unify_validate(elem)
                 for i, elem in enumerate(other)]
        return set.__ior__(self, other)

    # this redirects to __and__ anyway, so no need for tvu wrapper
    def __iand__(self, other):
        self._check_setter()
        if self._elem_validator is not None:
            validator = self._elem_validator
            other = \
                [validator('other[%s]' % (i,)).unify_validate(elem)
                 for i, elem in enumerate(other)]
        return set.__iand__(self, other)

    # this redirects to __sub__ anyway, so no need for tvu wrapper
    def __isub__(self, other):
        self._check_setter()
        if self._elem_validator is not None:
            validator = self._elem_validator
            other = \
                [validator('other[%s]' % (i,)).unify_validate(elem)
                 for i, elem in enumerate(other)]
        return set.__isub__(self, other)

    # this redirects to __xor__ anyway, so no need for tvu wrapper
    def __ixor__(self, other):
        self._check_setter()
        if self._elem_validator is not None:
            validator = self._elem_validator
            other = \
                [validator('other[%s]' % (i,)).unify_validate(elem)
                 for i, elem in enumerate(other)]
        return set.__ixor__(self, other)

    def __contains__(self, item):
        self._check_getter()
        return set.__contains__(self, item)

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __gt__(self, other):
        self._check_getter()
        return set.__gt__(self, other)

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __lt__(self, other):
        self._check_getter()
        return set.__lt__(self, other)

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __eq__(self, other):
        self._check_getter()

        if not isinstance(other, ScriveSet):
            return False

        return self._read_only == other._read_only and set.__eq__(self, other)

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __ne__(self, other):
        self._check_getter()

        if not isinstance(other, ScriveSet):
            return True

        return self._read_only != other._read_only or set.__ne__(self, other)

    def __len__(self):
        self._check_getter()
        return set.__len__(self)

    def __iter__(self):
        self._check_getter()
        return set.__iter__(self)

    def _set_read_only(self):
        for item in itertools.chain(
                set.__iter__(self),  # iterate even if self already invalid/ro
                iter(self._derived_objs)):
            if isinstance(item, _object.ScriveObject):
                item._set_read_only()
        super(ScriveSet, self)._set_read_only()

    def _set_invalid(self):
        for item in itertools.chain(
                set.__iter__(self),  # iterate even if self already invalid/ro
                iter(self._derived_objs)):
            if isinstance(item, _object.ScriveObject):
                item._set_invalid()
        super(ScriveSet, self)._set_invalid()

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __rxor__(self, other):
        self._check_getter()
        # proxy to __xor__, it's ok cause it's symmetric
        result = set.__xor__(self, other)
        result.__init_scrive_set__()
        _object.ScriveObject.__init__(result)
        self._derived_objs.append(result)
        return result

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __rand__(self, other):
        self._check_getter()
        # proxy to __and__, it's ok cause it's symmetric
        result = set.__and__(self, other)
        result.__init_scrive_set__()
        _object.ScriveObject.__init__(result)
        self._derived_objs.append(result)
        return result

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __ror__(self, other):
        self._check_getter()
        # proxy to __or__, it's ok cause it's symmetric
        result = set.__or__(self, other)
        result.__init_scrive_set__()
        _object.ScriveObject.__init__(result)
        self._derived_objs.append(result)
        return result

    @tvu.validate_and_unify(other=tvu.instance(set))
    def __rsub__(self, other):
        self._check_getter()
        # __sub__ isn't symmetric, we have to be creative
        result = ScriveSet(other)
        result -= self
        self._derived_objs.append(result)
        return result

    def get_by_attrs(self, **kwargs):
        '''
        Return first object matching all key=val attributes. or None.
        '''
        self._check_getter()
        for x in self:
            matches = True
            for key, val in kwargs.items():
                try:
                    if getattr(x, key) != val:
                        matches = False
                        break
                except AttributeError:
                    matches = False
                    break
            if matches:
                return x
        return None
