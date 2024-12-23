from collections import defaultdict

with open("aoc24/inputs/day20.txt") as f:
    grid = f.read().splitlines()

from functools import cache

# found_cheats = {}


def check_once():
    max_cheats = 2
    nodes = {
        (x, y, None, None, n)
        for x in range(len(grid))
        for y in range(len(grid))
        for n in range(max_cheats + 1)
    }
    best_score = defaultdict(lambda: float("inf"))
    previous = {}
    for x, row in enumerate(grid):
        for y, char in enumerate(row):
            if char == "S":
                start = (x, y, None, None, max_cheats)
            elif char == "E":
                exit = (x, y)

    best_score[start] = 0

    @cache
    def get_adjacent(x, y):
        return [
            (x + dx, y + dy)
            for dx, dy in ((0, 1), (0, -1), (1, 0), (-1, 0))
            if 0 <= x + dx < len(grid) and 0 <= y + dy < len(grid)
        ]

    @cache
    def get_neighbors(node):
        x, y, cheat_start, cheat_end, cheats_remaining = node
        in_wall_now = grid[x][y] == "#"

        neighbors = []
        for x2, y2 in get_adjacent(x, y):
            in_wall_after = grid[x2][y2] == "#"
            if not (in_wall_now or in_wall_after):
                neighbors.append((x2, y2, cheat_start, cheat_end, cheats_remaining))
            elif not cheats_remaining:
                continue
            elif in_wall_now and not in_wall_after:
                if True:  # (cheat_start, (x2, y2)) not in found_cheats:
                    neighbors.append((x2, y2, cheat_start, (x2, y2), 0))

            elif (not in_wall_now) and in_wall_after:
                neighbors.append((x2, y2, (x, y), None, cheats_remaining - 1))
            else:
                neighbors.append((x2, y2, cheat_start, cheat_end, cheats_remaining - 1))
        return neighbors

    nodes_list = sorted(nodes, key=lambda node: best_score[node])
    visited = set()
    while nodes:
        if len(visited) % 10000 == 0:
            print(f"Have visited {len(visited)} nodes, {len(nodes)} remaining")
        node = nodes_list.pop(0)
        nodes.remove(node)
        visited.add(node)

        re_sort = False
        neighbors = get_neighbors(node)
        for neighbor in neighbors:
            if neighbor in visited:
                continue
            if neighbor not in nodes:
                nodes.add(neighbor)
                nodes_list.append(neighbor)

            new_score = best_score[node] + 1
            if new_score < best_score[neighbor]:
                previous[neighbor] = node
                best_score[neighbor] = new_score
                re_sort = True
        if re_sort:
            nodes_list.sort(key=lambda node: best_score[node])

    exits = {k: v for k, v in best_score.items() if k[:2] == exit}
    best_exit = sorted(((k, v) for k, v in exits.items()), key=lambda x: x[1])[0]
    finish, score = best_exit
