import enum
from dateutil import parser as dateparser

import type_value_unifier as tvu
from scrivepy import _object, _field, _exceptions, _set, _file


scrive_property = _object.scrive_property
SFT = _field.StandardFieldType
SF = _field.StandardField


class InvitationDeliveryMethod(str, enum.Enum):
    email = 'email'
    pad = 'pad'
    api = 'api'
    mobile = 'mobile'
    email_and_mobile = 'email_mobile'


class ConfirmationDeliveryMethod(str, enum.Enum):
    email = 'email'
    mobile = 'mobile'
    email_and_mobile = 'email_mobile'
    none = 'none'


class AuthenticationMethod(str, enum.Enum):
    standard = 'standard'
    eleg = 'eleg'
    sms_pin = 'sms_pin'


class SignatoryAttachment(_object.ScriveObject):

    @tvu.validate_and_unify(requested_name=tvu.NonEmptyUnicode,
                            description=tvu.NonEmptyUnicode)
    def __init__(self, requested_name, description):
        super(SignatoryAttachment, self).__init__()
        self._requested_name = requested_name
        self._description = description
        self._file = None

    @classmethod
    def _from_json_obj(cls, json):
        try:
            signatory_attachment = \
                SignatoryAttachment(requested_name=json['name'],
                                    description=json['description'])
            file_json = json.get('file')
            if file_json is not None:
                file_ = _file.RemoteFile(id_=file_json['id'],
                                         name=file_json['name'])
                signatory_attachment._file = file_
            return signatory_attachment
        except (KeyError, TypeError, ValueError) as e:
            raise _exceptions.InvalidResponse(e)

    def _to_json_obj(self):
        return {'name': self.requested_name,
                'description': self.description}

    def _set_api(self, api, document):
        super(SignatoryAttachment, self)._set_api(api, document)
        if self.file is not None:
            self.file._set_api(api, document)

    def _set_invalid(self):
        if self.file is not None:
            self.file._set_invalid()
        super(SignatoryAttachment, self)._set_invalid()

    def _set_read_only(self):
        super(SignatoryAttachment, self)._set_read_only()
        if self.file is not None:
            self.file._set_read_only()

    @scrive_property
    def requested_name(self):
        return self._requested_name

    @requested_name.setter
    @tvu.validate_and_unify(requested_name=tvu.NonEmptyUnicode)
    def requested_name(self, requested_name):
        self._requested_name = requested_name

    @scrive_property
    def description(self):
        return self._description

    @description.setter
    @tvu.validate_and_unify(description=tvu.NonEmptyUnicode)
    def description(self, description):
        self._description = description

    @scrive_property
    def file(self):
        return self._file


IDM = InvitationDeliveryMethod
CDM = ConfirmationDeliveryMethod
AM = AuthenticationMethod

MaybeUnicode = tvu.nullable(tvu.instance(str))


class Signatory(_object.ScriveObject):

    @tvu.validate_and_unify(sign_order=tvu.PositiveInt,
                            invitation_delivery_method=
                            tvu.instance(IDM, enum=True),
                            confirmation_delivery_method=
                            tvu.instance(CDM, enum=True),
                            authentication_method=
                            tvu.instance(AM, enum=True),
                            viewer=tvu.instance(bool),
                            allows_highlighting=tvu.instance(bool),
                            sign_success_redirect_url=MaybeUnicode,
                            rejection_redirect_url=MaybeUnicode)
    def __init__(self, sign_order=1, viewer=False,
                 invitation_delivery_method=IDM.email,
                 confirmation_delivery_method=CDM.email,
                 authentication_method=AM.standard,
                 allows_highlighting=False,
                 sign_success_redirect_url=None,
                 rejection_redirect_url=None):
        super(Signatory, self).__init__()
        self._id = None
        self._current = None
        self._sign_order = sign_order
        self._undelivered_invitation = None
        self._undelivered_email_invitation = None
        self._undelivered_sms_invitation = None
        self._delivered_invitation = None
        self._has_account = None
        self._invitation_delivery_method = invitation_delivery_method
        self._confirmation_delivery_method = confirmation_delivery_method
        self._viewer = viewer
        self._allows_highlighting = allows_highlighting
        self._author = False
        self._eleg_mismatch_message = None
        self._sign_time = None
        self._view_time = None
        self._invitation_view_time = None
        self._rejection_time = None
        self._rejection_message = None
        self._sign_success_redirect_url = sign_success_redirect_url
        self._rejection_redirect_url = rejection_redirect_url
        self._authentication_method = authentication_method
        self._sign_url = None
        self._fields = _set.ScriveSet()
        self._fields._elem_validator = tvu.instance(_field.Field)
        self._attachments = _set.ScriveSet()
        self._attachments._elem_validator = tvu.instance(SignatoryAttachment)

    @classmethod
    def _from_json_obj(cls, json):
        try:
            fields = [_field.Field._from_json_obj(field_json)
                      for field_json in json['fields']]
            attachments = [SignatoryAttachment._from_json_obj(att_json)
                           for att_json in json['attachments']]
            signatory = \
                Signatory(sign_order=json['signorder'],
                          invitation_delivery_method=IDM(json['delivery']),
                          confirmation_delivery_method=CDM(
                              json['confirmationdelivery']),
                          authentication_method=AM(json['authentication']),
                          viewer=not json['signs'],
                          allows_highlighting=json['allowshighlighting'],
                          sign_success_redirect_url=
                          json['signsuccessredirect'],
                          rejection_redirect_url=json['rejectredirect'])
            signatory.fields.update(fields)
            signatory.attachments.update(attachments)
            signatory._id = json['id']
            signatory._author = json['author']
            signatory._current = json['current']
            signatory._undelivered_invitation = json['undeliveredInvitation']
            signatory._undelivered_email_invitation = \
                json['undeliveredMailInvitation']
            signatory._undelivered_sms_invitation = \
                json['undeliveredSMSInvitation']
            signatory._delivered_invitation = \
                json['deliveredInvitation']
            signatory._has_account = \
                json['saved']
            signatory._eleg_mismatch_message = \
                json['datamismatch']
            if json['signdate'] is not None:
                signatory._sign_time = dateparser.parse(json['signdate'])
            if json['seendate'] is not None:
                signatory._view_time = dateparser.parse(json['seendate'])
            if json['readdate'] is not None:
                signatory._invitation_view_time = \
                    dateparser.parse(json['readdate'])
            if json['rejecteddate'] is not None:
                signatory._rejection_time = \
                    dateparser.parse(json['rejecteddate'])
            signatory._rejection_message = json['rejectionreason']
            signatory._sign_url = json.get('signlink')
            return signatory
        except (KeyError, TypeError, ValueError) as e:
            raise _exceptions.InvalidResponse(e)

    def _set_invalid(self):
        # invalidate fields first, before getter stops working
        self.fields._set_invalid()
        self.attachments._set_invalid()
        super(Signatory, self)._set_invalid()

    def _set_read_only(self):
        super(Signatory, self)._set_read_only()
        self.fields._set_read_only()
        self.attachments._set_read_only()

    def _set_api(self, api, document):
        super(Signatory, self)._set_api(api, document)
        for attachment in self.attachments:
            attachment._set_api(api, document)

    def _to_json_obj(self):
        result = {'fields': list(self.fields),
                  'attachments': list(self.attachments),
                  'signorder': self.sign_order,
                  'delivery': self.invitation_delivery_method,
                  'confirmationdelivery':
                  self.confirmation_delivery_method,
                  'authentication': self.authentication_method,
                  'signs': not self.viewer,
                  'allowshighlighting': self.allows_highlighting,
                  'author': self.author,
                  'signsuccessredirect': self.sign_success_redirect_url,
                  'rejectredirect': self.rejection_redirect_url}
        if self.id is not None:
            result['id'] = self.id
        return result

#     @property
#     def status(self):
# documents.status                                == DocumentError => SCError           ~ "problem"
# documents.status                                == Preparation   => SCDraft           ~ "draft"
# signatory_links.sign_time                       != NULL          => SCSigned          ~ "signed"
# documents.status                                == Canceled      => SCCancelled       ~ "cancelled"
# documents.status                                == Timedout      => SCTimedout        ~ "timeouted"
# documents.status                                == Rejected      => SCRejected        ~ "rejected"
# signatory_links.seen_time                       != NULL          => SCOpened          ~ "opened"
# signatory_links.read_invitation                 != NULL          => SCRead            ~ "read"
# signatory_links.mail_invitation_delivery_status == Undelivered   => SCDeliveryProblem ~ "deliveryproblem"
# signatory_links.sms_invitation_delivery_status  == Undelivered   => SCDeliveryProblem ~ "deliveryproblem"
# signatory_links.mail_invitation_delivery_status == Delivered     => SCDelivered       ~ "delivered"
# signatory_links.sms_invitation_delivery_status  == Delivered     => SCDelivered       ~ "delivered"
# otherwise                                                        => SCSent            ~ "sent"

    @scrive_property
    def fields(self):
        return self._fields

    @scrive_property
    def attachments(self):
        return self._attachments

    @scrive_property
    def id(self):
        return self._id

    @scrive_property
    def current(self):
        return self._current

    @scrive_property
    def sign_order(self):
        return self._sign_order

    @sign_order.setter
    @tvu.validate_and_unify(sign_order=tvu.PositiveInt)
    def sign_order(self, sign_order):
        self._sign_order = sign_order

    @scrive_property
    def undelivered_invitation(self):
        return self._undelivered_invitation

    @scrive_property
    def undelivered_email_invitation(self):
        return self._undelivered_email_invitation

    @scrive_property
    def undelivered_sms_invitation(self):
        return self._undelivered_sms_invitation

    @scrive_property
    def delivered_invitation(self):
        return self._delivered_invitation

    @scrive_property
    def invitation_delivery_method(self):
        return self._invitation_delivery_method

    @invitation_delivery_method.setter
    @tvu.validate_and_unify(
        invitation_delivery_method=tvu.instance(IDM, enum=True))
    def invitation_delivery_method(self, invitation_delivery_method):
        self._invitation_delivery_method = invitation_delivery_method

    @scrive_property
    def confirmation_delivery_method(self):
        return self._confirmation_delivery_method

    @confirmation_delivery_method.setter
    @tvu.validate_and_unify(
        confirmation_delivery_method=tvu.instance(CDM, enum=True))
    def confirmation_delivery_method(self, confirmation_delivery_method):
        self._confirmation_delivery_method = confirmation_delivery_method

    @scrive_property
    def viewer(self):
        return self._viewer

    @viewer.setter
    @tvu.validate_and_unify(viewer=tvu.instance(bool))
    def viewer(self, viewer):
        self._viewer = viewer

    @scrive_property
    def allows_highlighting(self):
        return self._allows_highlighting

    @allows_highlighting.setter
    @tvu.validate_and_unify(allows_highlighting=tvu.instance(bool))
    def allows_highlighting(self, allows_highlighting):
        self._allows_highlighting = allows_highlighting

    @scrive_property
    def author(self):
        return self._author

    @scrive_property
    def has_account(self):
        return self._has_account

    @scrive_property
    def eleg_mismatch_message(self):
        return self._eleg_mismatch_message

    @scrive_property
    def sign_time(self):
        return self._sign_time

    @scrive_property
    def view_time(self):
        return self._view_time

    @scrive_property
    def invitation_view_time(self):
        return self._invitation_view_time

    @scrive_property
    def rejection_time(self):
        return self._rejection_time

    @scrive_property
    def rejection_message(self):
        return self._rejection_message

    @scrive_property
    def sign_success_redirect_url(self):
        return self._sign_success_redirect_url

    @sign_success_redirect_url.setter
    @tvu.validate_and_unify(sign_success_redirect_url=MaybeUnicode)
    def sign_success_redirect_url(self, sign_success_redirect_url):
        self._sign_success_redirect_url = sign_success_redirect_url

    @scrive_property
    def rejection_redirect_url(self):
        return self._rejection_redirect_url

    @rejection_redirect_url.setter
    @tvu.validate_and_unify(rejection_redirect_url=MaybeUnicode)
    def rejection_redirect_url(self, rejection_redirect_url):
        self._rejection_redirect_url = rejection_redirect_url

    @scrive_property
    def authentication_method(self):
        return self._authentication_method

    @authentication_method.setter
    @tvu.validate_and_unify(
        authentication_method=tvu.instance(AM, enum=True))
    def authentication_method(self, authentication_method):
        self._authentication_method = authentication_method

    @scrive_property
    def sign_url(self):
        return self._sign_url

    def absolute_sign_url(self):
        self._check_getter()
        if self._sign_url is None:
            return None
        if self._api is None:
            raise _exceptions.Error('API not set')
        proto = 'https' if self._api.https else 'http'
        return proto + '://' + self._api.api_hostname + self._sign_url

    @scrive_property
    def full_name(self):
        fst_name_fields = [f for f in self.fields if f.name == SFT.first_name]
        last_name_fields = [f for f in self.fields if f.name == SFT.last_name]
        first_name_part = ''
        try:
            first_name_part = fst_name_fields[0].value
        except IndexError:
            pass

        last_name_part = ''
        try:
            last_name_part = last_name_fields[0].value
        except IndexError:
            pass

        if first_name_part != '' and last_name_part != '':
            return first_name_part + ' ' + last_name_part
        elif first_name_part != '':
            return first_name_part
        elif last_name_part != '':
            return last_name_part
        else:
            return ''

    @full_name.setter
    @tvu.validate_and_unify(full_name=tvu.instance(str))
    def full_name(self, full_name):
        fst_name_fields = [f for f in self.fields if f.name == SFT.first_name]
        try:
            fst_name_field = fst_name_fields[0]
        except IndexError:
            fst_name_field = SF(name=SFT.first_name, value='')
            self.fields.add(fst_name_field)

        last_name_fields = [f for f in self.fields if f.name == SFT.last_name]
        try:
            last_name_field = last_name_fields[0]
        except IndexError:
            last_name_field = SF(name=SFT.last_name, value='')
            self.fields.add(last_name_field)

        if ' ' in full_name:
            name_parts = full_name.split(' ')
            fst_name = name_parts[0]
            last_name = ' '.join(name_parts[1:])
        elif full_name != ' ':
            fst_name = full_name
            last_name = ''
        else:
            fst_name = ''
            last_name = ''

        fst_name_field.value = fst_name
        last_name_field.value = last_name
