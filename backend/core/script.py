import os
import datetime
import re
import uuid
import shutil
import mimetypes
import hashlib

from django.conf import settings
from django.utils.timezone import make_aware
from core.models import Image, Album


def try_parse_image_date_from_file(fullpath):

    def try_parse_from_YYYYMMDD_hhmmss(s):
        regex = re.compile('([0-9]{4})([0-9]{2})([0-9]{2})_([0-9]{2})([0-9]{2})([0-9]{2})')
        match = regex.match(s)
        if (match):
            return datetime.datetime(*(map(int, match.groups())))
        else:
            return None

    filename = os.path.basename(fullpath)
    return try_parse_from_YYYYMMDD_hhmmss(filename)


class File:
    def __init__(self, fullpath):
        self.fullpath = fullpath
        self.image_date = None


def run(rootpath):

    print(f'Start indexing from {rootpath}...')

    files = []

    # with open('output.txt', 'w') as f:
    #     for dirpath, dirnames, filenames in os.walk('.'):
    #         for fullpath in [os.path.join(dirpath, filename) for filename in filenames]:
    #             f.write(f'{fullpath}\n')

    for dirpath, dirnames, filenames in os.walk(rootpath):
        for filename in filenames:
            fullpath = os.path.join(dirpath, filename)
            if 'image' not in mimetypes.guess_type(fullpath)[0]:
                continue

            file = File(fullpath)
            file.image_date = try_parse_image_date_from_file(fullpath)

            files.append(file)
        break

    files.sort(key=lambda file: (file.image_date is None, file.image_date))

    for file in files:
        print(file.image_date, file.fullpath)

    files = files[0:10]

    for file in files:
        src = file.fullpath
        id = uuid.uuid4()
        _, file_extension = os.path.splitext(fullpath)
        new_filename = f'{id}{file_extension}'
        dst = os.path.join(settings.BASE_DIR, settings.MEDIA_ROOT, new_filename)

        print(f'Copying from {src} to {dst}')
        shutil.copyfile(src, dst)

        file.new_fullpath = dst
        file.id = id

    album = Album.objects.get(name='image_indexer')

    for file in files:
        image = Image(
            id=file.id,
            album=album,
            original=os.path.basename(file.new_fullpath),
            image_date=make_aware(file.image_date),
            source=os.path.relpath(file.fullpath, rootpath),
            sha256_checksum=get_file_sha256(file.new_fullpath),
        )
        image.save()


def get_file_sha256(fullpath):
    h = hashlib.sha256()
    with open(fullpath, 'rb') as file:
        while True:
            chunk = file.read(h.block_size)
            if not chunk:
                break
            h.update(chunk)
    return h.hexdigest()
