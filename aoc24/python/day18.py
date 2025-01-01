from collections import defaultdict

with open("aoc24/inputs/day18.txt") as f:
    walls = f.read().splitlines()


border = 70

for number in [1024, *range(len(walls), 1024, -1)]:
    nodes = []
    best_score = defaultdict(lambda: float("inf"))
    previous = defaultdict(set)
    exits = []

    for x in range(border + 1):
        for y in range(border + 1):
            if f"{y},{x}" not in walls[:number]:
                nodes.append((x, y))

    start = (0, 0)
    best_score[start] = 0
    exit = (border, border)

    def get_neighbors(node):
        return [
            (node[0] + dx, node[1] + dy)
            for dx, dy in ((0, 1), (0, -1), (1, 0), (-1, 0))
        ]

    nodes.sort(key=lambda node: best_score[node])
    while nodes:
        node = nodes.pop(0)

        re_sort = False
        for neighbor in get_neighbors(node):
            if neighbor not in nodes:
                continue
            new_score = best_score[node] + 1
            if new_score <= best_score[neighbor]:
                previous[neighbor].add(node)
            if new_score < best_score[neighbor]:
                best_score[neighbor] = new_score
                re_sort = True
        if re_sort:
            nodes.sort(key=lambda node: best_score[node])

    path_length = best_score[exit]
    if number == 1024:
        print("Part 1:", path_length)
    elif path_length != float("inf"):
        print("Part 2:", walls[number])
        break
