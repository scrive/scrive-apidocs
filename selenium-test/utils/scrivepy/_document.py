import os
from io import BytesIO
from contextlib import closing

import enum
from dateutil import parser as dateparser

import type_value_unifier as tvu
from scrivepy import _object, _signatory, _exceptions, \
    _set, _file, _unicode_dict


scrive_property = _object.scrive_property


class DocumentStatus(str, enum.Enum):
    preparation = 'Preparation'
    pending = 'Pending'
    closed = 'Closed'
    canceled = 'Canceled'
    timedout = 'Timedout'
    rejected = 'Rejected'
    error = 'DocumentError'


MaybeUnicode = tvu.nullable(tvu.instance(str))


class Language(str, enum.Enum):
    english = 'en'
    swedish = 'sv'
    german = 'de'
    french = 'fr'
    italian = 'it'
    spanish = 'es'
    portuguese = 'pt'
    dutch = 'nl'
    danish = 'da'
    norwegian = 'no'
    greek = 'el'
    finnish = 'fi'
    estonian = 'et'
    islandic = 'is'
    lithuanian = 'lt'
    latvian = 'lv'
    czech = 'cs'
    polish = 'pl'


class DeletionStatus(enum.Enum):
    not_deleted = 0
    in_trash = 1
    deleted = 2


class AuthorAttachment(_file.File):

    @tvu.validate_and_unify(name=tvu.NonEmptyUnicode,
                            content=tvu.instance(bytes),
                            mandatory=tvu.instance(bool),
                            merge=tvu.instance(bool))
    def __init__(self, name, content, mandatory=False, merge=True):
        super(AuthorAttachment, self).__init__(name)
        self._content = content
        self._mandatory = mandatory
        self._merge = merge

    def stream(self):
        return BytesIO(self._content)

    @classmethod
    def from_file_obj(cls, name, file_obj):
        with closing(file_obj) as f:
            return AuthorAttachment(name, f.read())

    @classmethod
    def from_file_path(cls, file_path):
        with open(file_path, 'rb') as f:
            return AuthorAttachment.from_file_obj(
                str(os.path.basename(file_path)), f)

    @scrive_property
    def mandatory(self):
        return self._mandatory

    @mandatory.setter
    @tvu.validate_and_unify(mandatory=tvu.instance(bool))
    def mandatory(self, mandatory):
        self._mandatory = mandatory

    @scrive_property
    def merge(self):
        return self._merge

    @merge.setter
    @tvu.validate_and_unify(merge=tvu.instance(bool))
    def merge(self, merge):
        self._merge = merge


class RemoteAuthorAttachment(_file.RemoteFile):

    @tvu.validate_and_unify(id_=_object.ID,
                            name=tvu.NonEmptyUnicode,
                            mandatory=tvu.instance(bool),
                            merge=tvu.instance(bool))
    def __init__(self, id_, name, mandatory=False, merge=True):
        super(RemoteAuthorAttachment, self).__init__(id_, name)
        self._mandatory = mandatory
        self._merge = merge

    @classmethod
    def _from_json_obj(cls, json):
        return RemoteAuthorAttachment(id_=json['id'],
                                      name=json['name'],
                                      mandatory=json['required'],
                                      merge=json['add_to_sealed_file'])

    @scrive_property
    def mandatory(self):
        return self._mandatory

    @mandatory.setter
    @tvu.validate_and_unify(mandatory=tvu.instance(bool))
    def mandatory(self, mandatory):
        self._mandatory = mandatory

    @scrive_property
    def merge(self):
        return self._merge

    @merge.setter
    @tvu.validate_and_unify(merge=tvu.instance(bool))
    def merge(self, merge):
        self._merge = merge


class Document(_object.ScriveObject):

    @tvu.validate_and_unify(title=tvu.instance(str),
                            number_of_days_to_sign=tvu.bounded_int(1, 90),
                            number_of_days_to_remind=
                            tvu.nullable(tvu.PositiveInt),
                            is_template=tvu.instance(bool),
                            show_header=tvu.instance(bool),
                            show_pdf_download=tvu.instance(bool),
                            show_reject_option=tvu.instance(bool),
                            show_reject_reason=tvu.instance(bool),
                            show_footer=tvu.instance(bool),
                            invitation_message=MaybeUnicode,
                            confirmation_message=MaybeUnicode,
                            api_callback_url=MaybeUnicode,
                            language=tvu.instance(Language, enum=True),
                            saved_as_draft=tvu.instance(bool),
                            timezone=tvu.instance(str))
    def __init__(self, title='', number_of_days_to_sign=14,
                 number_of_days_to_remind=None,
                 show_header=True, show_pdf_download=True,
                 show_reject_option=True, show_reject_reason=True,
                 show_footer=True, invitation_message=None,
                 confirmation_message=None, api_callback_url=None,
                 language=Language.swedish, is_template=False,
                 saved_as_draft=False, timezone='Europe/Stockholm'):
        super(Document, self).__init__()
        self._id = None
        self._title = title
        self._number_of_days_to_sign = number_of_days_to_sign
        self._number_of_days_to_remind = number_of_days_to_remind
        self._status = None
        self._modification_time = None
        self._creation_time = None
        self._signing_deadline = None
        self._autoremind_time = None
        self._current_sign_order = None
        self._is_template = is_template
        self._show_header = show_header
        self._show_pdf_download = show_pdf_download
        self._show_reject_option = show_reject_option
        self._show_reject_reason = show_reject_reason
        self._show_footer = show_footer
        self.invitation_message = invitation_message  # setter has better logic
        self.confirmation_message = \
            confirmation_message  # setter has better logic
        self._api_callback_url = api_callback_url
        self._language = language
        self._tags = _unicode_dict.UnicodeDict()
        self._saved_as_draft = saved_as_draft
        self._deletion_status = DeletionStatus.not_deleted
        self._signing_possible = None
        self._object_version = None
        self._timezone = timezone
        self._viewed_by_author = None
        self._access_token = None
        self._signatories = _set.ScriveSet()
        self._signatories._elem_validator = tvu.instance(_signatory.Signatory)
        self._original_file = None
        self._sealed_document = None
        self._author_attachments = _set.ScriveSet()
        self._author_attachments._elem_validator = \
            tvu.instance(AuthorAttachment)

    @classmethod
    def _from_json_obj(cls, json):
        try:
            signatories = [_signatory.Signatory._from_json_obj(signatory_json)
                           for signatory_json in json['signatories']]
            lang_code = json['lang']
            if lang_code == 'gb':
                lang_code = 'en'
            document = Document(title=json['title'],
                                number_of_days_to_sign=json['daystosign'],
                                number_of_days_to_remind=json['daystoremind'],
                                is_template=json['template'],
                                show_header=json['showheader'],
                                show_pdf_download=json['showpdfdownload'],
                                show_reject_option=json['showrejectoption'],
                                show_reject_reason=json['allowrejectreason'],
                                show_footer=json['showfooter'],
                                invitation_message=json['invitationmessage'],
                                confirmation_message=
                                json['confirmationmessage'],
                                api_callback_url=json['apicallbackurl'],
                                language=Language(lang_code),
                                saved_as_draft=json['saved'],
                                timezone=json['timezone'])
            document.signatories.update(signatories)
            document.tags.update({elem['name']: elem['value']
                                  for elem in json['tags']})
            document._id = json['id']
            if json['time'] is not None:
                document._modification_time = dateparser.parse(json['time'])
            if json['ctime'] is not None:
                document._creation_time = dateparser.parse(json['ctime'])
            if json['timeouttime'] is not None:
                document._signing_deadline = \
                    dateparser.parse(json['timeouttime'])
            if json['autoremindtime'] is not None:
                document._autoremind_time = \
                    dateparser.parse(json['autoremindtime'])
            document._status = DocumentStatus(json['status'])
            document._current_sign_order = json['signorder']
            deleted = json['deleted']
            really_deleted = json['reallydeleted']
            if deleted and really_deleted:
                document._deletion_status = DeletionStatus.deleted
            elif deleted:
                document._deletion_status = DeletionStatus.in_trash
            document._signing_possible = json['canperformsigning']
            document._object_version = json['objectversion']
            document._viewed_by_author = json['isviewedbyauthor']
            document._access_token = json['accesstoken']
            document._original_file = \
                _file.RemoteFile._from_json_obj(json.get('file'))
            document._sealed_document = \
                _file.RemoteFile._from_json_obj(json.get('sealedfile'))
            author_attachments = \
                _set.ScriveSet([RemoteAuthorAttachment._from_json_obj(att_json)
                                for att_json in json['authorattachments']])
            author_attachments._elem_validator = tvu.instance(AuthorAttachment)
            document._author_attachments = author_attachments

            if document.status is not DocumentStatus.preparation:
                document._set_read_only()

            return document
        except (KeyError, TypeError, ValueError) as e:
            raise _exceptions.InvalidResponse(e, json)

    def _set_invalid(self):
        # invalidate subobjects first, before getter stops working
        self.signatories._set_invalid()
        self.tags._set_invalid()
        self.author_attachments._set_invalid()
        if self.original_file is not None:
            self.original_file._set_invalid()
        if self.sealed_document is not None:
            self.sealed_document._set_invalid()
        super(Document, self)._set_invalid()

    def _set_read_only(self):
        super(Document, self)._set_read_only()
        self.signatories._set_read_only()
        self.tags._set_read_only()
        self.author_attachments._set_read_only()
        if self.original_file is not None:
            self.original_file._set_read_only()
        if self.sealed_document is not None:
            self.sealed_document._set_read_only()

    def _to_json_obj(self):
        return {'title': self.title,
                'daystosign': self.number_of_days_to_sign,
                'daystoremind': self.number_of_days_to_remind,
                'template': self.is_template,
                'showheader': self.show_header,
                'showpdfdownload': self.show_pdf_download,
                'showrejectoption': self.show_reject_option,
                'allowrejectreason': self.show_reject_reason,
                'showfooter': self.show_footer,
                'invitationmessage': self.invitation_message or '',
                'confirmationmessage': self.confirmation_message or '',
                'apicallbackurl': self.api_callback_url,
                'lang': self.language,
                'tags': [{'name': key, 'value': val}
                          for key, val in self.tags.items()],
                'saved': self.saved_as_draft,
                'timezone': self.timezone,
                'authorattachments': list(self.author_attachments),
                'signatories': list(self.signatories)}

    @scrive_property
    def signatories(self):
        return self._signatories

    def other_parties(self):
        '''
        Return all signatories except the author.
        '''
        self._check_getter()
        for s in self._signatories:
            if not s.author:
                yield s

    def other_signatories(self):
        '''
        Return all signing signatories except the author.
        '''
        self._check_getter()
        for s in self._signatories:
            if not s.author and not s.viewer:
                yield s

    def other_signatory(self):
        '''
        Return non-author signing signatory (if there's just one).

        Raises errors, if there's less/more than one.
        '''
        self._check_getter()
        others = list(self.other_signatories())
        if not others:
            raise _exceptions.Error('No other signatories')
        elif len(others) > 1:
            raise _exceptions.Error('Multiple signatories')
        else:
            return others[0]

    @scrive_property
    def id(self):
        return self._id

    @scrive_property
    def title(self):
        return self._title

    @title.setter
    @tvu.validate_and_unify(title=tvu.instance(str))
    def title(self, title):
        self._title = title

    @scrive_property
    def number_of_days_to_sign(self):
        return self._number_of_days_to_sign

    @number_of_days_to_sign.setter
    @tvu.validate_and_unify(number_of_days_to_sign=tvu.bounded_int(1, 90))
    def number_of_days_to_sign(self, number_of_days_to_sign):
        self._number_of_days_to_sign = number_of_days_to_sign

    @scrive_property
    def status(self):
        return self._status

    @scrive_property
    def modification_time(self):
        return self._modification_time

    @scrive_property
    def creation_time(self):
        return self._creation_time

    @scrive_property
    def signing_deadline(self):
        return self._signing_deadline

    @scrive_property
    def autoremind_time(self):
        return self._autoremind_time

    @scrive_property
    def current_sign_order(self):
        return self._current_sign_order

    @scrive_property
    def authentication_method(self):
        signatories = list(self.signatories)
        if not signatories:
            return 'mixed'

        # at least 1 signatory
        first_signatory = signatories.pop(0)
        result = first_signatory.authentication_method
        for signatory in signatories:
            if signatory.authentication_method != result:
                # signatories use various auth methods
                return 'mixed'
        # all signatories have the same auth method
        return result.value

    @scrive_property
    def invitation_delivery_method(self):
        signatories = list(self.signatories)
        if not signatories:
            return 'mixed'

        # at least 1 signatory
        first_signatory = signatories.pop(0)
        result = first_signatory.invitation_delivery_method
        for signatory in signatories:
            if signatory.invitation_delivery_method != result:
                # signatories use various invitation delivery methods
                return 'mixed'
        # all signatories have the same invitation delivery method
        return result.value

    @scrive_property
    def is_template(self):
        return self._is_template

    @is_template.setter
    @tvu.validate_and_unify(is_template=tvu.instance(bool))
    def is_template(self, is_template):
        self._is_template = is_template

    @scrive_property
    def number_of_days_to_remind(self):
        return self._number_of_days_to_remind

    @number_of_days_to_remind.setter
    @tvu.validate_and_unify(
        number_of_days_to_remind=tvu.nullable(tvu.PositiveInt))
    def number_of_days_to_remind(self, number_of_days_to_remind):
        self._number_of_days_to_remind = number_of_days_to_remind

    @scrive_property
    def show_header(self):
        return self._show_header

    @show_header.setter
    @tvu.validate_and_unify(show_header=tvu.instance(bool))
    def show_header(self, show_header):
        self._show_header = show_header

    @scrive_property
    def show_pdf_download(self):
        return self._show_pdf_download

    @show_pdf_download.setter
    @tvu.validate_and_unify(show_pdf_download=tvu.instance(bool))
    def show_pdf_download(self, show_pdf_download):
        self._show_pdf_download = show_pdf_download

    @scrive_property
    def show_reject_option(self):
        return self._show_reject_option

    @show_reject_option.setter
    @tvu.validate_and_unify(show_reject_option=tvu.instance(bool))
    def show_reject_option(self, show_reject_option):
        self._show_reject_option = show_reject_option

    @scrive_property
    def show_reject_reason(self):
        return self._show_reject_reason

    @show_reject_reason.setter
    @tvu.validate_and_unify(show_reject_reason=tvu.instance(bool))
    def show_reject_reason(self, show_reject_reason):
        self._show_reject_reason = show_reject_reason

    @scrive_property
    def show_footer(self):
        return self._show_footer

    @show_footer.setter
    @tvu.validate_and_unify(show_footer=tvu.instance(bool))
    def show_footer(self, show_footer):
        self._show_footer = show_footer

    @scrive_property
    def invitation_message(self):
        return self._invitation_message

    @invitation_message.setter
    @tvu.validate_and_unify(invitation_message=MaybeUnicode)
    def invitation_message(self, invitation_message):
        if invitation_message is not None and invitation_message.isspace()\
           or invitation_message == '':
            invitation_message = None
        self._invitation_message = invitation_message

    @scrive_property
    def confirmation_message(self):
        return self._confirmation_message

    @confirmation_message.setter
    @tvu.validate_and_unify(confirmation_message=MaybeUnicode)
    def confirmation_message(self, confirmation_message):
        if confirmation_message is not None and confirmation_message.isspace()\
           or confirmation_message == '':
            confirmation_message = None
        self._confirmation_message = confirmation_message

    @scrive_property
    def api_callback_url(self):
        return self._api_callback_url

    @api_callback_url.setter
    @tvu.validate_and_unify(api_callback_url=MaybeUnicode)
    def api_callback_url(self, api_callback_url):
        self._api_callback_url = api_callback_url

    @scrive_property
    def language(self):
        return self._language

    @language.setter
    @tvu.validate_and_unify(language=tvu.instance(Language, enum=True))
    def language(self, language):
        self._language = language

    @scrive_property
    def tags(self):
        return self._tags

    @scrive_property
    def saved_as_draft(self):
        return self._saved_as_draft

    @saved_as_draft.setter
    @tvu.validate_and_unify(saved_as_draft=tvu.instance(bool))
    def saved_as_draft(self, saved_as_draft):
        self._saved_as_draft = saved_as_draft

    @scrive_property
    def deletion_status(self):
        return self._deletion_status

    @scrive_property
    def signing_possible(self):
        return self._signing_possible

    @scrive_property
    def object_version(self):
        return self._object_version

    @scrive_property
    def timezone(self):
        return self._timezone

    @timezone.setter
    @tvu.validate_and_unify(timezone=tvu.instance(str))
    def timezone(self, timezone):
        self._timezone = timezone

    @scrive_property
    def viewed_by_author(self):
        return self._viewed_by_author

    @scrive_property
    def access_token(self):
        return self._access_token

    @scrive_property
    def original_file(self):
        return self._original_file

    @scrive_property
    def sealed_document(self):
        return self._sealed_document

    @scrive_property
    def author_attachments(self):
        return self._author_attachments

    @scrive_property
    def author(self):
        authors = [s for s in self._signatories if s.author]
        if not authors:
            raise _exceptions.Error('No author')
        if len(authors) > 1:
            raise _exceptions.Error('Multiple authors')
        else:
            return authors[0]

    def _set_api(self, api, _document):
        super(Document, self)._set_api(api, self)
        if self.original_file is not None:
            self.original_file._set_api(api, self)
        if self.sealed_document is not None:
            self.sealed_document._set_api(api, self)
        for file_ in self.author_attachments:
            file_._set_api(api, self)
        for signatory in self.signatories:
            signatory._set_api(api, self)
