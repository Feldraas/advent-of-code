from collections import defaultdict

with open("aoc24/inputs/day06.txt") as f:
    lines = f.read().splitlines()

size = len(lines)

obstacles = {}
for x in range(size):
    for y in range(size):
        if lines[x][y] == "#":
            obstacles[(x, y)] = "#"
        elif lines[x][y] == "^":
            start_pos = (x, y)

up = (-1, 0)
down = (1, 0)
left = (0, -1)
right = (0, 1)

start_dir = up


def turn_right(direction):
    return {up: right, right: down, down: left, left: up}[direction]


def trace(pos, direction, obstacles: dict):
    path = defaultdict(set)

    while True:
        if 0 in pos or size in pos:
            return path, "edge"

        path[pos].add(direction)

        next_pos = (
            pos[0] + direction[0],
            pos[1] + direction[1],
        )

        if next_pos in obstacles:
            direction = turn_right(direction)
            continue

        elif next_pos in path and direction in path[next_pos]:
            return path, "loop"

        pos = next_pos


part_1 = part_2 = 0

for x in range(size):
    for y in range(size):  # Cringe
        if lines[x][y] == "#":
            continue

        if (x, y) == start_pos:
            path, _ = trace(start_pos, up, obstacles)
            part_1 = len(path)
        else:
            _, reason = trace(start_pos, up, obstacles | {(x, y): "#"})
            if reason == "loop":
                part_2 += 1

print(f"Part 1: {part_1}")
print(f"Part 2: {part_2}")
