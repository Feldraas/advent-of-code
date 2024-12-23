from functools import cache

with open("aoc24/inputs/day19.txt") as f:
    patterns, _, *designs = f.read().splitlines()
    patterns = patterns.split(", ")


@cache
def arrangements(design: str):
    if design == "":
        return 1

    return sum(
        arrangements(design.replace(pattern, "", 1))
        for pattern in patterns
        if design.startswith(pattern)
    )


arrs = [arrangements(design) for design in designs]
arrs = [arr for arr in arrs if arr > 0]


print("Part 1:", len(arrs))
print("Part 2:", sum(arrs))
