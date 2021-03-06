#!/usr/bin/env python3


import glob
import json
import os
import os.path
import sys

import requests


def get_or_create_release(tag, session):
    get_resp = session.get("https://api.github.com/repos/benjamin-hodgson/es-profile-visualiser/releases/tags/" + tag)
    if get_resp.status_code == 404:
        # release doesn't exist yet
        print("Creating release")
        post_resp = session.post(
            "https://api.github.com/repos/benjamin-hodgson/es-profile-visualiser/releases",
            data=json.dumps({"tag_name": tag}),
        )
        return post_resp.json()["upload_url"]
    else:
        print("Release already exists")
        return get_resp.json()["upload_url"]

    
def upload_assets(upload_url, session):
    for path in glob.glob("artifacts/*"):
        filename = os.path.basename(path)

        content_type = "application/zip" if filename.endswith("zip") else "application/gzip"

        with open(path, 'rb') as file:
            print("Adding asset to release")
            resp = session.post(upload_url + "?name=" + filename, file.read(), headers={"Content-Type": content_type})
            resp.raise_for_status()


def main():
    tag = sys.argv[1]
    tok = sys.argv[2]

    with requests.Session() as session:
        session.auth = ("benjamin-hodgson", tok)

        upload_url = get_or_create_release(tag, session)

        upload_assets(upload_url.rpartition('{')[0], session)


if __name__ == "__main__":
    main()
    