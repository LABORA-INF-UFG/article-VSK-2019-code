import time
from locust import HttpLocust, TaskSet, task
import pickle
from gevent.pool import Pool
import random
from locust.wait_time import constant
from load_test.util.session_util import SessionUtil
import load_test.util.constants as constants
import os
# with open('sessions.pickle', 'rb') as sessions_pickle:
#     sessions = pickle.load(sessions_pickle)

filename = os.environ.get("SESSION_FILE")
print("filename = ./load_test/{}".format(filename))
num_lines = sum(1 for line in open("./load_test/{}".format(filename)))

def readSession(sessionId):
    return SessionUtil.parse_session_line("./load_test/{}".format(filename), sessionId)


class CallTile:

    def __init__(self, locust, tile):
        self.tile = tile
        self.locust = locust

    def call_tile(self):
        self.locust.client.get("/styles/osm-bright/{}.png".format(self.tile))


class WMSUser(TaskSet):

    # def on_start(self):
    #     print(self.locust.__dict__)
    #     if len(sessions) > 0:
    #         self.session = random.choice(sessions)
    #         print("CHOOSED SESSION = {}".format(self.session.id))

    @task
    def front_page(self):
        sessionId = random.randint(1, num_lines)
        self.session = readSession(sessionId)
        #print("CHOOSED SESSION = {}, length: {}".format(self.session.id, len(self.session.actions_list)))
        called_tiles = []
        for action in self.session.actions_list:
            if action.tiles:
                group = Pool(6)
                for tile in action.tiles:
                    if tile not in called_tiles:
                        call = CallTile(self, tile)
                        group.spawn(call.call_tile)
                        called_tiles.append(tile)
                    else:
                        print("Cached tile: {}".format(tile))
                group.join()

            #print("Session ID: {}, Interval: {}".format(self.session.id, action.interval/1000))

            time.sleep(action.interval/1000)


class MapUser(HttpLocust):
    task_set = WMSUser
    wait_time = constant(0)
    host = constants.SERVER
