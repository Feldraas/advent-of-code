import re

with open("aoc24/inputs/day03.txt") as f:
    instructions = f.read().replace("\n", "")


def total_sum(instructions: str) -> int:
    mults = re.findall(r"mul\(\d+,\d+\)", instructions)

    nums = [re.findall(r"\d+", ins) for ins in mults]
    return sum(int(num[0]) * int(num[1]) for num in nums)


def drop_disabled(instructions: str):
    instructions2 = instructions.replace("do()", "\ndo()")
    instructions2 = instructions2.replace("don't()", "\ndon't()")
    instructions2 = instructions2.split("\n")
    return "".join(ins for ins in instructions2 if not ins.startswith("don't()"))


part_1 = total_sum(instructions)
part_2 = total_sum(drop_disabled(instructions))

print(f"Part 1: {part_1}")
print(f"Part 2: {part_2}")
