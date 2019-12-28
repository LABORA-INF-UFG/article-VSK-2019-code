import erss.util.constants as constants
from erss.model.bounding_box import BoundingBox
from erss.model.pixel import Pixel
from erss.model.tile import Tile

from math import log
from math import tan
from math import cos
from math import pi


class TileUtil:

    @staticmethod
    def coordinates_to_tile_frist_step(coordinates):
        lat = coordinates[0]
        lon = coordinates[1]
        zoom = coordinates[2]

        if abs(lat) > 180 or abs(lon) > 180:
            return None, None

        xc = (lon + 180)/360 * 2**zoom

        yc = (1 - (log(tan(lat * (pi/180)) + 1/cos(lat * (pi/180))))/pi) * 2**(zoom-1)

        return xc, yc

    @staticmethod
    def coordinates_to_tile(coordinates):
        xc, yc = TileUtil.coordinates_to_tile_frist_step(coordinates)

        return int(xc), int(yc)

    @staticmethod
    def coordinates_to_pixel(coordinates):
        xc, yc = TileUtil.coordinates_to_tile_frist_step(coordinates)
        z = coordinates[2]

        if xc is None or yc is None:
            return None

        return Tile(int(xc * constants.TILE_SIZE), int(yc * constants.TILE_SIZE), z)

    @staticmethod
    def list_tiles(central_coordinates, resolution):
        if None in central_coordinates or resolution == "" or 'undefined' in resolution:
            return None

        res_x = int(resolution.split("x")[0])
        res_y = int(resolution.split("x")[1])

        central_pixel = TileUtil.coordinates_to_pixel(central_coordinates)

        if central_pixel is None:
            return None

        left_x = central_pixel.x - int(res_x/2)
        right_x = central_pixel.x + int(res_x / 2)
        top_y = central_pixel.y - int(res_y / 2)
        bottom_y = central_pixel.y + int(res_y / 2)

        if (res_x % 2) == 0:
            right_x += 1

        if (res_y % 2) == 0:
            bottom_y += 1

        tiles_list = []
        z = central_pixel.z
        x = left_x
        while x <= right_x or int(x/constants.TILE_SIZE) == int(right_x/constants.TILE_SIZE):
            y = top_y
            while y <= bottom_y or int(y/constants.TILE_SIZE) == int(bottom_y/constants.TILE_SIZE):
                tile = Tile(int(x/constants.TILE_SIZE), int(y/constants.TILE_SIZE), z)

                if tile.x < 0 or tile.y > 2**(z) - 1:
                    tile.x = tile.x % 2**z
                    if tile.x < 0:
                        tile.x = tile.x + 2**z

                if tile.y < 0 or tile.y > 2**z - 1:
                    tile.y = tile.y % 2**z
                    if tile.y < 0:
                        tile.y = tile.y + 2**z

                tiles_list.append(tile)

                y = y + constants.TILE_SIZE

            x = x + constants.TILE_SIZE

        return tiles_list




