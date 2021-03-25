
inputFile = open('input.txt').read().split('\n')[:-2]

tileStrings = [inputFile[i:(i + 11)] for i in range(0, len(inputFile), 12)]

class Tile:
    end = 9
    def __init__(self, data):
        tileNumber, *tiles = data
        self.number = int(tileNumber[-5:-1])
        self.tiles = tiles
        self.potentialLeft = []
        self.potentialRight = []
        self.potentialUp = []
        self.potentialDown = []
        self.rotation = 0
        self.transposed = False

    def rotate(self):
        length = 10
        self.rotation += 1
        # newTile = [[None for _ in range(0, length)] for _ in range(0, length)]
        newTile = [[self.tiles[j][i] for j in range(length)] for i in range(length - 1, -1, -1)]
        # for i in range(length - 1, -1, -1):
        #     for j in range(0, length):
        #         newTile[i][j] = self.tiles[j][i]
        self.tiles = newTile

    def transpose(self):
        length = 10
        self.transposed = not self.transposed
        newTile = [[self.tiles[j][i] for j in range(length)] for i in range(length)]
        self.tiles = newTile

    def __str__(self):
        tiles = ''
        for tile in self.tiles:
            tiles += '\n' + tile
        return str(self.number) + tiles

    def matchRight(self, otherTile):
        for index, value in enumerate(self.tiles):
            if not value[self.end] == otherTile.tiles[index][0]:
                return False
        return True
    def matchLeft(self, otherTile):
        for index, value in enumerate(self.tiles):
            if not value[0] == otherTile.tiles[index][self.end]:
                return False
        return True
    def matchUp(self, otherTile):
        for index, value in enumerate(self.tiles[0]):
            if not value == otherTile.tiles[self.end][index]:
                return False
        return True
    def matchDown(self, otherTile):
        for index, value in enumerate(self.tiles[self.end]):
            if not value == otherTile.tiles[0][index]:
                return False
        return True

class Board:
    def __init__(self, tiles):
        self.tiles = tiles

    def findNeighbors(self):
        for tile in self.tiles:
            for otherTile in self.tiles:
                if not tile == otherTile:
                    for _ in range(2):
                        if tile.matchDown(otherTile):
                            tile.potentialDown.append(otherTile)
                            otherTile.potentialUp.append(tile)
                        if tile.matchUp(otherTile):
                            tile.potentialUp.append(otherTile)
                            otherTile.potentialDown.append(tile)
                        if tile.matchLeft(otherTile):
                            tile.potentialLeft.append(otherTile)
                            otherTile.potentialRight.append(tile)
                        if tile.matchRight(otherTile):
                            tile.potentialRight.append(otherTile)
                            otherTile.potentialLeft.append(tile)
                        tile.transpose()
    def getCorners(self):
        corners = []
        for tile in self.tiles:
            # count = 0
            # count += len(tile.potentialUp)
            # count += len(tile.potentialDown)
            # count += len(tile.potentialLeft)
            # count += len(tile.potentialRight)
            # if tile.potentialDown: count += 1
            # if tile.potentialUp: count += 1
            # if tile.potentialLeft: count += 1
            # if tile.potentialRight: count += 1
            if tile.potentialDown == [] and tile.potentialLeft is None:
                corners.append(tile)
            elif tile.potentialDown is None and tile.potentialRight is None:
                corners.append(tile)
            elif tile.potentialUp is None and tile.potentialRight is None:
                corners.append(tile)
            elif tile.potentialUp is None and tile.potentialLeft is None:
                corners.append(tile)
            # if count == 2: corners.append(tile)
        return corners

tileLists = [Tile(ls) for ls in tileStrings]
board = Board(tileLists)

board.findNeighbors()
print(len(board.tiles))

print(len(board.getCorners()))

