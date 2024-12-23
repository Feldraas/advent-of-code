with open("aoc24/inputs/day05.txt") as f:
    lines = f.read().splitlines()

split = lines.index("")

rules_str = lines[:split]
updates_str = lines[split + 1 :]

rules = []
for rule in rules_str:
    rules.append([int(n) for n in rule.split("|")])


updates = []
for update in updates_str:
    updates.append([int(n) for n in update.split(",")])


def is_relevant(rule, update):
    return set(rule).issubset(set(update))


def is_correct_for_rule(update, rule):
    try:
        return update.index(rule[0]) < update.index(rule[1])
    except ValueError:
        return True


def is_correct(update):
    return all(is_correct_for_rule(update, rule) for rule in rules)


def get_relevant_rules(update):
    return [rule for rule in rules if is_relevant(rule, update)]


def merge_rules(rules):
    uniques = set(sum(rules, start=[]))
    right_numbers = {rule[1] for rule in rules}
    leftmost = list(uniques - right_numbers)[0]
    rules_without_leftmost = [rule for rule in rules if leftmost not in rule]

    if len(right_numbers) == 1:
        return [leftmost, *right_numbers]

    return [leftmost, *merge_rules(rules_without_leftmost)]


def fix(update):
    merged_rules = merge_rules(get_relevant_rules(update))

    return [num for num in merged_rules if num in update]


part_1 = part_2 = 0
for update in updates:
    if is_correct(update):
        part_1 += update[int(len(update) / 2)]
    else:
        fixed = fix(update)
        part_2 += fixed[int(len(fixed) / 2)]

print(f"Part 1: {part_1}")
print(f"Part 2: {part_2}")
