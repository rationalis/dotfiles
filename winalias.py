#!/usr/bin/env python

import os
import shutil

exe_files = []

s = '''#!/bin/sh
{} "$@"'''

for pathdir in os.getenv("PATH").split(os.pathsep):
    if os.path.isdir(pathdir):
        for file in os.listdir(pathdir):
            if file.endswith('.exe') and shutil.which(file[:-4]) is None:
                exe_files.append((file[:-4], os.path.join(pathdir, file)))

for alias, path in exe_files:
    new_path = 'win/' + alias
    with open(new_path, 'w') as f:
        f.write(s.format(path))
    os.chmod(new_path, os.stat(new_path).st_mode | 0o111)
