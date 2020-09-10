# coding: utf-8
import inspect

import enum


class TypeValueUnifier(object):

    def __init__(self, variable_name=None):
        self._variable_name = \
            variable_name or self.__class__.__name__ + '() argument'

    def type_check(self):
        value = self._value
        types = self.__class__.TYPES
        if isinstance(value, types):
            return

        if len(types) == 1:
            possible_types_string = types[0].__name__
        else:
            possible_types_string = \
                ', '.join([type_.__name__ for type_ in types[:-1]]) \
                + ' or ' + types[-1].__name__

        err_msg = '%s must be %s, not %s' % (self._variable_name,
                                              possible_types_string,
                                              str(self._value))
        raise TypeError(err_msg)

    def unify_validate(self, value):
        self._value = value
        self.type_check()
        unified_value = self.unify(self._value)
        self.validate(unified_value)
        return unified_value

    def error(self, msg, soft=False):
        word = 'could' if soft else 'must'
        full_msg = ('%s %s be ' + msg + ', not: %s') % \
            (self._variable_name, word, str(self._value))
        raise ValueError(full_msg)

    def unify(self, value):
        return value

    def validate(self, value):
        pass


def validate_and_unify(**arg_validators):
    def wrapper(fun):
        spec_args, spec_varargs, spec_keywords, spec_defaults = \
            inspect.getargspec(fun)

        def inner_wrapper(*args, **kwargs):
            args_values = inspect.getcallargs(fun, *args, **kwargs)
            for arg in args_values:
                try:
                    validator = arg_validators[arg]
                except KeyError:
                    pass
                else:
                    args_values[arg] = \
                        validator(arg).unify_validate(args_values[arg])

            new_args = []

            # TODO: support default arguments
            try:
                for arg in spec_args:
                    new_args.append(args_values.pop(arg))
                if spec_varargs is not None:
                    new_args += args_values.pop(spec_varargs)
            except KeyError:
                raise TypeError()
            if spec_keywords is not None:
                kwargs = args_values.pop(spec_keywords)
                for key, val in kwargs.items():
                    args_values[key] = val

            return fun(*new_args, **args_values)
        return inner_wrapper
    return wrapper


class EnumTypeValueUnifier(TypeValueUnifier):

    def _get_enum_type(self):
        for type_ in self.TYPES:
            if issubclass(type_, enum.Enum):
                return type_

    def type_check(self):
        value = self._value
        if isinstance(value, str) and not isinstance(value, enum.Enum):
            enum_type = self._get_enum_type()
            try:
                self._value = getattr(enum_type, value)
            except AttributeError:
                msg = enum_type.__name__ + "'s variant name"
                self.error(msg, soft=True)
            return
        super(EnumTypeValueUnifier, self).type_check()


def instance(class_, enum=False):
    base = EnumTypeValueUnifier if enum else TypeValueUnifier

    class InstanceTypeValueUnifier(base):
        TYPES = (class_,)

    return InstanceTypeValueUnifier


def nullable(tvu):

    class NullableTypeValueUnifier(tvu):
        TYPES = tvu.TYPES + (type(None),)

        def unify(self, value):
            if value is None:
                return value
            return super(NullableTypeValueUnifier, self).unify(value)

        def validate(self, value):
            if value is None:
                return
            super(NullableTypeValueUnifier, self).validate(value)

        def error(self, err_msg):
            err_msg = 'None or ' + err_msg
            return super(NullableTypeValueUnifier, self).error(err_msg)

    return NullableTypeValueUnifier


def bounded_int(minimum=None, maximum=None):

    class BoundedInt(TypeValueUnifier):

        TYPES = (int, float)

        def __init__(self, variable_name=None):
            self._variable_name = \
                variable_name or self.__class__.__name__ + '() argument'

        def unify(self, value):
            if isinstance(value, float) and round(value) != value:
                self.error('a round integer')
            return int(value)

        def validate(self, value):
            if minimum is not None and maximum is not None\
               and not (minimum <= value <= maximum):
                err_msg = \
                    'an integer between %d and %d (inclusive)' % (minimum,
                                                                   maximum)
                self.error(err_msg)
            elif minimum is not None and value < minimum:
                err_msg = 'an integer greater or equal than %d' % (minimum,)
                self.error(err_msg)
            elif maximum is not None and value > maximum:
                err_msg = 'an integer lesser or equal than %d' % (maximum,)
                self.error(err_msg)

    return BoundedInt

PositiveInt = bounded_int(1)


class Iterable(TypeValueUnifier):

    def type_check(self):
        value = self._value
        try:
            iter(value)
            return
        except TypeError:
            err_msg = '%s must be iterable, not %s' % (self._variable_name,
                                                        str(self._value))
            raise TypeError(err_msg)


def args_of(tvu):

    class ArgsOf(TypeValueUnifier):

        def type_check(self):
            value = self._value
            try:
                iter(value)
            except TypeError:
                err_msg = '%s must be iterable, not %s' % \
                    (self._variable_name, str(self._value))
                raise TypeError(err_msg)

        def unify(self, value):
            result = []
            for i, elem in enumerate(value):
                new_var_name = '%s[%d]' % (self._variable_name, i)
                validated_elem = tvu(new_var_name).unify_validate(elem)
                result.append(validated_elem)
            return result

    return ArgsOf


class NonEmptyUnicode(TypeValueUnifier):

    TYPES = (str,)

    def validate(self, value):
        if value == '':
            self.error('non-empty string')


class Text(TypeValueUnifier):

    TYPES = (str,)

    def unify(self, value):
        if isinstance(value, str):
            try:
                return value.decode('ascii')
            except UnicodeDecodeError:
                self.error('unicode text, or ascii-only bytestring')
        return value
