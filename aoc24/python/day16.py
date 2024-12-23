from collections import defaultdict

with open("aoc24/inputs/day16.txt") as f:
    grid = f.read().splitlines()


UP = (-1, 0)
DOWN = (1, 0)
LEFT = (0, -1)
RIGHT = (0, 1)

dirs = [UP, DOWN, LEFT, RIGHT]


nodes = []
best_score = defaultdict(lambda: float("inf"))
previous = defaultdict(set)
exits = []


for x, row in enumerate(grid):
    for y, char in enumerate(row):
        if char == "#":
            continue

        nodes.extend((x, y, dir) for dir in dirs)

        if char == "S":
            start = (x, y, RIGHT)
            best_score[start] = 0
        elif char == "E":
            exits.extend((x, y, dir) for dir in dirs)


def score(node1, node2):
    return 1 if node1[2] == node2[2] else 1001


def opposite(dir):
    return {UP: DOWN, DOWN: UP, LEFT: RIGHT, RIGHT: LEFT}[dir]


def get_neighbors(node):
    return [
        (node[0] + dir[0], node[1] + dir[1], dir)
        for dir in dirs
        if dir != opposite(node[2])
    ]


nodes.sort(key=lambda node: best_score[node])
while nodes:
    node = nodes.pop(0)

    re_sort = False
    for neighbor in get_neighbors(node):
        if neighbor not in nodes:
            continue
        new_score = best_score[node] + score(node, neighbor)
        if new_score <= best_score[neighbor]:
            previous[neighbor].add(node)
        if new_score < best_score[neighbor]:
            best_score[neighbor] = new_score
            re_sort = True
    if re_sort:
        nodes.sort(key=lambda node: best_score[node])


part_1 = min(best_score[exit] for exit in exits)


paths = {(exit,) for exit in exits if best_score[exit] == part_1}
copy = {}

while paths != copy:
    copy = paths.copy()
    for path in copy:
        if path[0] == start:
            continue
        for prev in previous[path[0]]:
            paths.add((prev, *path))
        paths.remove(path)


def total_score(path):
    return sum(score(path[i], path[i + 1]) for i in range(len(path) - 1))


best_tiles = set()
for path in paths:
    if total_score(path) != part_1:
        continue
    for tile in path:
        best_tiles.add(tile[:2])

part_2 = len(best_tiles)

print(f"Part 1: {part_1}")
print(f"Part 2: {part_2}")
