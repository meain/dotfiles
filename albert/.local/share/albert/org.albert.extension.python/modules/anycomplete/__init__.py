# -*- coding: utf-8 -*-

"""Figure out the spelling of a word using google autocomplete

Synopsis: <trigger> <filter>"""

from albert import *
from locale import getdefaultlocale
from urllib import request, parse
import json
import time
import os
from socket import timeout

__title__ = "Anycomplete"
__version__ = "0.0.1"
__triggers__ = "ac "
__authors__ = "meain"

iconPath = os.path.dirname(__file__) + "/anycomplete.svg"
baseurl = "https://suggestqueries.google.com/complete/search?client=firefox&q="
searchURL = "https://duckduckgo.com/?q="  # use ddg for actual search
limit = 20


def handleQuery(query):
    if query.isTriggered:
        query.disableSort()

        # avoid rate limiting
        time.sleep(0.1)
        if not query.isValid:
            return

        stripped = query.string.strip()

        if stripped:
            results = []

            get_url = "%s?%s" % (baseurl, parse.quote(stripped))
            req = request.Request(get_url)

            with request.urlopen(req) as response:
                data = json.loads(response.read().decode("utf-8"))

                for i in range(0, min(limit, len(data[1]))):
                    title = data[1][i]
                    url = searchURL + title

                    results.append(
                        Item(
                            id=__title__,
                            icon=iconPath,
                            text=title,
                            completion=title,
                            actions=[
                                ClipAction("Copy completion", title),
                                UrlAction("Start search", url),
                            ],
                        )
                    )

            return results
        else:
            return Item(
                id=__title__,
                icon=iconPath,
                text=__title__,
                subtext="Enter to autocomplete using Google",
            )
