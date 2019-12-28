import re

class URLUtil:

    @staticmethod
    def extract_coordinates(url):
        coordinate_match = re.search(r'/@.*(\.|).*,.\d*(\.|)\d*z', url)
        lat = None
        long = None
        z = None
        if coordinate_match:
            coordinate = coordinate_match.group()
            lat = coordinate.split(",")[0].replace("/@", "")
            long = coordinate.split(",")[1]
            z = coordinate.split(",")[2].replace("z", "")
            if z is not None:
                z = int(float(z))
                lat = float(lat)
                long = float(long)
        coordinate_string = "{}/{}/{}".format(z, lat, long)
        return lat, long, z, coordinate_string

    @staticmethod
    def check_if_is_street_map(url):
        coordinate_match = re.search(r'@.*\..*,\d*z', url)

        return coordinate_match is not None

    @staticmethod
    def check_if_is_street_view(url):
        coordinate_match = re.search(r'@.*,.*a,.*y,', url)

        return coordinate_match is not None

    @staticmethod
    def check_if_is_satellite_map(url):
        coordinate_match = re.search(r'@.*\..*,\d*m', url)

        return coordinate_match is not None

