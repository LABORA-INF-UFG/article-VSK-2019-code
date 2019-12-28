
class Tile:

    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    def __str__(self):
        return "{}/{}/{}".format(self.z, self.x, self.y)

    def __repr__(self):
        return "{}/{}/{}".format(self.z, self.x, self.y)
