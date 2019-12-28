import os
import json
from collections import namedtuple


class FileUtil:

    @staticmethod
    def read_json_file(filename):
        with open(filename, 'r') as f:
            content = json.load(f)

        return content

    @staticmethod
    def list_directories(dirname):
        return next(os.walk(dirname))[1]




