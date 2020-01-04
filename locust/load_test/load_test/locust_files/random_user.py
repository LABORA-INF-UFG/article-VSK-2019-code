import time
from locust import HttpLocust, TaskSet, task
import pickle
from gevent.pool import Pool
import random
from locust.wait_time import constant
from load_test.util.session_util import SessionUtil
import load_test.util.constants as constants
import os

class Tile:

    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    def __str__(self):
        return "{}/{}/{}".format(self.z, self.x, self.y)


MIN_ZOOM = 11
MAX_ZOOM = 20

class TilesToLook:

    zoom = MIN_ZOOM

    tiles_to_look = [
        Tile(602, 768, zoom),
        Tile(602, 769, zoom),
        Tile(601, 768, zoom),
        Tile(601, 769, zoom),
        Tile(602, 767, zoom)
    ]


total_number_of_tiles = 0

for z in range(0, MAX_ZOOM - MIN_ZOOM + 1):
    total_number_of_tiles += pow(4, z)


def find_zoom(tile_id):
    cum_number_of_tiles_in_zoom = 1
    choosed_zoom = 0
    if tile_id > 0:
        for z in range(1, MAX_ZOOM - MIN_ZOOM + 2):
            #print("tile_id = {}, cum_number_of_tiles_in_zoom = {}".format(tile_id,cum_number_of_tiles_in_zoom))
            if tile_id <= cum_number_of_tiles_in_zoom:
                choosed_zoom = z - 1
                break
            cum_number_of_tiles_in_zoom += pow(4, z)
    
    return choosed_zoom, cum_number_of_tiles_in_zoom


class CallTile:

    def __init__(self, locust, tile):
        self.tile = tile
        self.locust = locust

    def call_tile(self):
        self.locust.client.get("/styles/osm-bright/{}.png".format(self.tile))


class RandomCalls(TaskSet):

    def call_tile(self, tile):
        url = "/styles/osm-bright/{}.png".format(tile)
        print(url)
        return url

    def generateTiles(self, tilesPerCall):
        tileList = []
        for t in range(0, tilesPerCall):
            base_tile_id = random.randint(0, len(TilesToLook.tiles_to_look) - 1)
            tile_id = random.randint(0, total_number_of_tiles)

            zoom, last_zoom_num_tiles = find_zoom(tile_id)

            rest = tile_id % last_zoom_num_tiles

            base_tile = TilesToLook.tiles_to_look[base_tile_id]

            y = base_tile.y * pow(2, zoom) + int(rest/pow(2, zoom))
            x = base_tile.x * pow(2, zoom) + rest % pow(2, zoom)

            #print("last_zoom_num_tiles = {}, tile_id = {}, zoom = {}, rest = {}, x = {}, y = {}".format(last_zoom_num_tiles, tile_id, zoom, rest, x, y))
            
            tileList.append("{}/{}/{}".format(zoom + MIN_ZOOM, int(x), int(y)))

        return tileList

    @task
    def front_page(self):
        num_tiles_per_call = int(os.environ.get("NUM_TILES_PER_CALL"))
        tiles_list = self.generateTiles(num_tiles_per_call)

        called_tiles = []

        group = Pool(6)
        for tile in tiles_list:
            #if tile not in called_tiles:
            call = CallTile(self, tile)
            group.spawn(call.call_tile)
            called_tiles.append(tile)
            
        group.join()


class RandomUser(HttpLocust):
    task_set = RandomCalls
    wait_time = constant(0)
    host = constants.SERVER
