with open("aoc24/inputs/day09.txt") as f:
    fs = f.read()[:-1]

# fs = "2333133121414131402"


def parse(fs, part):
    out = []

    file = 0
    i = 0

    while True:
        if len(fs) == 0:
            return out

        if part == 1:
            out.extend(int(fs[i]) * [file])
        else:
            out.append((int(fs[i]), file))
        if len(fs) == 1:
            return out

        if part == 1:
            out.extend(int(fs[i + 1]) * ["."])
        else:
            out.append((int(fs[i + 1]), "."))
        file += 1
        fs = fs[2:]


def compress_1(pfs):
    pfs = pfs[:]
    left = 0
    right = len(pfs) - 1

    while left != right:
        if pfs[left] != ".":
            left += 1
            continue
        if pfs[right] == ".":
            right -= 1
            continue

        pfs[left] = pfs[right]
        pfs[right] = "."

    return pfs


def checksum(cfs):
    return sum(i * int(el) for i, el in enumerate(cfs) if el != ".")


pfs = parse(fs, part=1)
cfs_1 = compress_1(pfs)

part_1 = checksum(cfs_1)


def gap_size_at(pfs, idx):
    if idx < 0 or idx >= len(pfs) or pfs[idx] != ".":
        return 0
    r = gap_size_at(pfs, idx + 1) + 1
    return r


def get_first_gap(pfs, freq):
    for i in range(len(pfs)):
        if set(pfs[i : i + freq]) == {"."}:
            return i
    return -1


def compress_2(pfs):
    pfs = pfs[:]
    for file_no in range(max(el for el in pfs if el != "."), 0, -1):
        freq = pfs.count(file_no)
        first_occurrence = pfs.index(file_no)

        first_gap = get_first_gap(pfs, freq)
        if -1 < first_gap < first_occurrence:
            pfs[first_occurrence : first_occurrence + freq] = freq * ["."]
            pfs[first_gap : first_gap + freq] = freq * [file_no]

    return pfs


cfs_2 = compress_2(pfs)
part_2 = checksum(cfs_2)


print(f"Part 1: {part_1}")
print(f"Part 2: {part_2}")
