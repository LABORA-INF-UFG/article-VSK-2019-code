import json
from erss.util.url_util import URLUtil


class URL:

    def __init__(self, url, timestamp, resolution, tabId):
        self.url = url
        self.coordinate = URLUtil.extract_coordinates(url)
        self.timestamp = timestamp
        self.resolution = resolution
        self.tabId = tabId

    def __str__(self):
        return json.dumps(self.__dict__)

    def __repr__(self):
        return json.dumps(self.__dict__)

    @staticmethod
    def url_dict_to_obj(url):
        uri = url['Url']
        timestamp = url['Timestamp']
        resolution = url['Resolution']
        tabId = url['TabId']

        url_obj = URL(uri, timestamp, resolution, tabId)

        return url_obj


