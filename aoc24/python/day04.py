with open("aoc24/inputs/day04.txt") as f:
    grid = f.read().splitlines()


def char_at(x, y):
    if x < 0 or y < 0:
        return "."
    try:
        return grid[x][y]
    except IndexError:
        return "."


def read_in_direction(x, y, dx, dy):
    return "".join((char_at(x + n * dx, y + n * dy) for n in range(4)))


def xmas_count(x, y):
    if char_at(x, y) != "X":
        return 0

    directions = [
        [dx, dy] for dx in (-1, 0, 1) for dy in (-1, 0, 1) if dx != 0 or dy != 0
    ]

    return len(
        [
            direction
            for direction in directions
            if read_in_direction(x, y, *direction) == "XMAS"
        ]
    )


def read_cross(x, y):
    return {
        char_at(x - 1, y - 1) + char_at(x, y) + char_at(x + 1, y + 1),
        char_at(x + 1, y - 1) + char_at(x, y) + char_at(x - 1, y + 1),
    }


nrows = len(grid)
ncols = len(grid[0])

part_1 = part_2 = 0
for x in range(nrows):
    for y in range(ncols):
        part_1 += xmas_count(x, y)
        if read_cross(x, y).issubset({"MAS", "SAM"}):
            part_2 += 1

print(f"Part 1: {part_1}")
print(f"Part 2: {part_2}")
