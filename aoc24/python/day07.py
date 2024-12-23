with open("aoc24/inputs/day07.txt") as f:
    lines = f.read().splitlines()


def parse(line):
    result, rest = line.split(": ")
    return int(result), [int(n) for n in rest.split(" ")]


def unconcat(result, num):
    return int(str(result)[: -len(str(num))])


def is_achievable(result, numbers, part):
    if isinstance(result, float) and not result.is_integer():
        return False

    result = int(result)

    if len(numbers) == 1:
        return result == numbers[0]

    return (
        is_achievable(result - numbers[-1], numbers[:-1], part)
        or is_achievable(result / numbers[-1], numbers[:-1], part)
        or (
            part == 2
            and (str(result).endswith(str(numbers[-1])))
            and is_achievable(unconcat(result, numbers[-1]), numbers[:-1], part)
        )
    )


part_1 = part_2 = 0

for line in lines:
    result, numbers = parse(line)
    if is_achievable(result, numbers, part=1):
        part_1 += result
        part_2 += result
    elif is_achievable(result, numbers, part=2):
        part_2 += result


print(f"Part 1: {part_1}")
print(f"Part 2: {part_2}")
