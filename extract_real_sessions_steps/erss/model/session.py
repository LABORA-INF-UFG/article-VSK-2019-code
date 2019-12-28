import json

class Session:

    def __init__(self, user, date, urls, steps, steps_tiles, app_id):
        self.user = user
        self.date = date
        self.urls = urls
        self.steps = steps
        self.steps_tiles = steps_tiles
        self.app_id = app_id

    def __str__(self):
        return str(self.__dict__)

