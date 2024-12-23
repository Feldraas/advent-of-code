with open("aoc24/inputs/day01.txt") as f:
    lines = f.read().splitlines()

left = []
right = []

for line in lines:
    a, b = line.split("   ")
    left.append(int(a))
    right.append(int(b))

left.sort()
right.sort()

part_1 = part_2 = 0

for i in range(len(left)):
    part_1 += abs(left[i] - right[i])
    part_2 += left[i] * right.count(left[i])

print(f"Part 1: {part_1}")
print(f"Part 2: {part_2}")
