import os
import mimetypes

rootpath = '/home/hectorhon/repo/lens/backend/data/staging/photos/'

fullpaths = []

for dirpath, dirnames, filenames in os.walk(rootpath):
    for filename in filenames:
        fullpath = os.path.join(dirpath, filename)
        type, encoding = mimetypes.guess_type(fullpath)
        if type is None or 'image' not in type:
            continue
        else:
            fullpaths.append(fullpath)

size = 0

for fullpath in fullpaths:
    size += os.path.getsize(fullpath)

print(size/10**9)
