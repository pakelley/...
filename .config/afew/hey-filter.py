#!/usr/bin/env python3

from pathlib import Path
import re

from afew.filters.BaseFilter import Filter
from afew.FilterRegistry import register_filter
from afew.utils import get_sender
from afew.NotmuchSettings import get_notmuch_new_tags, get_notmuch_new_query

import orgparse
import pandas

ORG_CONTACTS_PATH = Path("~/.local/share/notes/contacts.org").expanduser()
# from this SE answer: https://stackoverflow.com/a/201378/5054505
EMAIL_REGEX = "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])"
NAMED_EMAIL_REGEX = f"(?P<address>{EMAIL_REGEX})"


# TODO make method that generates a sieve filter for this logic
@register_filter
class HeyFilter(Filter):
    message = 'Tagging all mail with HEY categories'

    def __init__(self, database):
        super().__init__(database)

        with open(str(ORG_CONTACTS_PATH), "r") as f:
            contacts = orgparse.load(f)

        self.contact_group = {node.properties.get("EMAIL"): node.properties.get("EMAIL-GROUP", "screener")
                              for node in contacts.children
                              if node.properties.get("EMAIL")}
        self.addresses = [node.properties.get("EMAIL")
                          for node in contacts.children
                          if node.properties.get("EMAIL")]

    @property
    def query(self):
        '''
        Need to read the notmuch settings first. Using a property here
        so that the setting is looked up on demand.
        '''
        return get_notmuch_new_query()

    # @staticmethod
    def get_address(self, message):
        reply_to = message.get_header("Reply-To").strip()
        # gotta be a better syntax, but haven't bothered with it yet
        match = re.match(f".*<{NAMED_EMAIL_REGEX}>.*", reply_to) or re.match(NAMED_EMAIL_REGEX, reply_to)

        if match:
            sender = match.group("address")
            return sender
        else:
            # TODO see what's going on with these
            self.add_tags(message, "not-parsed")
            # print(f">>>>>ERROR>>>>>>: {reply_to}")

    def handle_message(self, message):
        # sender = get_sender(message)
        sender = self.get_address(message)
        if sender:
            print(f"Sender: {sender}")
            group = self.contact_group.get(sender, "screener")
            self.add_tags(message, group)
            self.remove_tags(message, *get_notmuch_new_tags())
