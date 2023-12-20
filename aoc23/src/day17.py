from numpy import inf
import itertools as it

part = 1

with open("aoc23/inputs/day17.txt") as f:
    inp = [[int(y) for y in x] for x in f.read().split("\n")]

x_max = len(inp) - 1
y_max = len(inp[0]) - 1

heat_loss = dict(zip(range(x_max + 1), it.repeat({})))
for i in range(x_max + 1):
    for j in range(y_max + 1):
        heat_loss[i, j] = inp[i][j]

directions = ["L", "R", "U", "D"]


def get_neighbor(node, direction_out):
    steps_in = node[0]
    direction_in = node[1]
    x = node[2][0]
    y = node[2][1]

    if {direction_in, direction_out} in [{"U", "D"}, {"L", "R"}]:
        return None

    if part == 1 and direction_in == direction_out and steps_in == 3:
        return None

    if part == 2 and (x, y) != (0, 0) and direction_in != direction_out and steps_in < 4:
        return None
    if part == 2 and direction_in == direction_out and steps_in == 10:
        return None

    if direction_out == "L":
        neighbor_coords = (x, y - 1)
    elif direction_out == "R":
        neighbor_coords = (x, y + 1)
    elif direction_out == "U":
        neighbor_coords = (x - 1, y)
    else:
        neighbor_coords = (x + 1, y)

    if (0 <= neighbor_coords[0] <= x_max) and (0 <= neighbor_coords[1] <= y_max):
        return (1 if direction_in != direction_out else steps_in + 1), direction_out, neighbor_coords


def get_neighbors(node):
    all_neighbors = [get_neighbor(node, direction) for direction in directions]
    return [nb for nb in all_neighbors if nb is not None]


nodes = dict()
seen_nodes = []

for steps_in in range(1, (4 if part == 1 else 11)):
    for direction_in in directions:
        for x in range(x_max + 1):
            for y in range(y_max + 1):
                nodes[(steps_in, direction_in, (x, y))] = get_neighbors((steps_in, direction_in, (x, y)))

if part == 2:
    delete = []
    for key in nodes:
        if key[-1] == (0, 0) or (key[-1] == (x_max, y_max) and key[0] < 4):
            delete.append(key)
    for key in delete:
        del nodes[key]
    nodes[0, None, (0, 0)] = get_neighbors((0, None, (0, 0)))

current_min_heat_loss = dict(zip(tuple(nodes), it.repeat(inf)))
previous_node = dict(zip(tuple(nodes), it.repeat(None)))

for key in current_min_heat_loss:
    if key[-1] == (0, 0):
        current_min_heat_loss[key] = 0
        seen_nodes.append(key)

seen_nodes = sorted(seen_nodes, key=lambda x: current_min_heat_loss[x], reverse=True)
changed_order = False

while seen_nodes:
    current = seen_nodes.pop()
    neighbors = nodes[current]
    del nodes[current]

    for neighbor in neighbors:
        if neighbor in nodes:
            new_heat_loss = current_min_heat_loss[current] + heat_loss[neighbor[2]]
            if new_heat_loss < current_min_heat_loss[neighbor]:
                current_min_heat_loss[neighbor] = new_heat_loss
                previous_node[neighbor] = current
                seen_nodes.append(neighbor)
                changed_order = True

    if changed_order:
        seen_nodes = sorted(set(seen_nodes), key=lambda x: current_min_heat_loss[x], reverse=True)
        changed_order = False

final_nodes = [key for key in current_min_heat_loss if key[-1] == (x_max, y_max) and current_min_heat_loss[key] != inf]
final_paths = [current_min_heat_loss[key] for key in final_nodes]

print(final_nodes)
print(final_paths)
print("Minimum heat loss:", min(final_paths))
