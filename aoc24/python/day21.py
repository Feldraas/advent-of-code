from collections import defaultdict
from functools import cache

code = "029A"
adjacent = {
    "A": list("03"),
    "0": list("A2"),
    "1": list("24"),
    "2": list("0135"),
    "3": list("A26"),
    "4": list("157"),
    "5": list("2468"),
    "6": list("359"),
    "7": list("48"),
    "8": list("579"),
    "9": list("68"),
    "^": list("vP"),
    "<": list("v"),
    ">": list("vP"),
    "v": list("<^>"),
    "P": list("^>"),
}

up = {
    "A": "3",
    "0": "2",
    "1": "4",
    "2": "5",
    "3": "6",
    "4": "7",
    "5": "8",
    "6": "9",
    "7": "7",
    "8": "8",
    "9": "9",
    "^": "^",
    "<": "<",
    ">": "P",
    "v": "^",
    "P": "P",
}
down = {
    "A": "A",
    "0": "0",
    "1": "1",
    "2": "0",
    "3": "A",
    "4": "1",
    "5": "2",
    "6": "3",
    "7": "4",
    "8": "5",
    "9": "6",
    "^": "v",
    "<": "<",
    ">": ">",
    "v": "v",
    "P": ">",
}

left = {
    "A": "0",
    "0": "0",
    "1": "1",
    "2": "1",
    "3": "2",
    "4": "4",
    "5": "4",
    "6": "5",
    "7": "7",
    "8": "8",
    "9": "9",
    "^": "^",
    "<": "<",
    ">": "v",
    "v": "<",
    "P": "^",
}
right = {
    "A": "A",
    "0": "A",
    "1": "2",
    "2": "3",
    "3": "3",
    "4": "5",
    "5": "6",
    "6": "6",
    "7": "8",
    "8": "9",
    "9": "9",
    "^": "P",
    "<": "v",
    ">": ">",
    "v": ">",
    "P": "P",
}

dir_map = {"^": up, "v": down, "<": left, ">": right}


def score(code):
    nodes = {
        (d1, d2, n, code[:i])
        for d1 in "<v>^P"
        for d2 in "<v>^P"
        for n in "0123456789A"
        for i in range(len(code) + 1)
    }
    best_score = defaultdict(lambda: float("inf"))
    previous = {}
    start = ("P", "P", "A", "")
    end = ("P", "P", "A", code)
    best_score[start] = 0

    @cache
    def get_neighbors(node):
        dpad1, dpad2, button, text = node

        neighbors = []
        # Left
        neighbors.append((left[dpad1], dpad2, button, text))
        # Right
        neighbors.append((right[dpad1], dpad2, button, text))
        # Up
        neighbors.append((up[dpad1], dpad2, button, text))
        # Down
        neighbors.append((down[dpad1], dpad2, button, text))
        # Push
        if dpad1 == "P" and dpad2 == "P":
            neighbors.append((dpad1, dpad2, button, text + button))
        elif dpad1 == "P":
            neighbors.append((dpad1, dpad2, dir_map[dpad2][button], text))
        else:
            neighbors.append((dpad1, dir_map[dpad1][dpad2], button, text))
        return neighbors

    nodes_list = sorted(nodes, key=lambda node: best_score[node])
    while nodes:
        node = nodes_list.pop(0)
        nodes.remove(node)

        re_sort = False
        neighbors = get_neighbors(node)
        for neighbor in neighbors:
            if neighbor not in nodes:
                continue

            new_score = best_score[node] + 1
            if new_score < best_score[neighbor]:
                previous[neighbor] = node
                best_score[neighbor] = new_score
                re_sort = True
        if re_sort:
            nodes_list.sort(key=lambda node: best_score[node])

    return best_score[end] * int(code[:-1])


with open("aoc24/inputs/day21.txt") as f:
    codes = f.read().splitlines()

part_1 = sum(score(code) for code in codes)
print("Part 1:", part_1)
