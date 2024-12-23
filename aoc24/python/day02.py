with open("aoc24/inputs/day02.txt") as f:
    lines = f.read().splitlines()

reports = [[int(i) for i in line.split(" ")] for line in lines]


def is_safe(report: list[int]) -> bool:
    diffs = {report[i] - report[i + 1] for i in range(len(report) - 1)}
    return diffs.issubset({1, 2, 3}) or diffs.issubset({-1, -2, -3})


def dampened_is_safe(report: list[int]) -> bool:
    if is_safe(report):
        return True

    for i in range(len(report)):
        copy = report[:]
        copy.pop(i)
        if is_safe(copy):
            return True

    return False


part_1 = len([report for report in reports if is_safe(report)])
part_2 = len([report for report in reports if dampened_is_safe(report)])

print(f"Part 1: {part_1}")
print(f"Part 2: {part_2}")
