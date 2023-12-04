from math import prod

def main():
    with open("inputs/day03.txt", "r") as f:
        lines = list(map(lambda l: l.strip(), f.readlines()))
        (sum_part_numbers, sum_gear_ratios) = solve(lines)
        print(f"part 1: {sum_part_numbers}\npart 2: {sum_gear_ratios}")

def solve(engine_schematic: list[str]) -> tuple[int, int]:
    current_number = ""
    current_adjacents = []
    nums_with_adjacent_symbol = []

    # {coordinate of gear symbol: {numbers adjacent}}
    potential_gears: dict[tuple[int, int], set[int]] = {}

    for i in range(0, len(engine_schematic)):
        for j in range(0, len(engine_schematic)):
            if engine_schematic[i][j].isdigit():
                current_number += engine_schematic[i][j]
                current_adjacents += adjacents((i, j), len(engine_schematic))
            else:
                adj_values = list(map(lambda tpl: engine_schematic[tpl[0]][tpl[1]], current_adjacents))

                # part 1
                if len(list(filter(is_symbol, adj_values))) > 0:
                    nums_with_adjacent_symbol.append(int(current_number))

                # part 2
                for (x, y) in current_adjacents:
                    if engine_schematic[x][y] == "*":
                        if (x, y) in potential_gears:
                            potential_gears[(x, y)].add(int(current_number))
                        else:
                            potential_gears[(x, y)] = {int(current_number)}

                current_number = ""
                current_adjacents = []

    gear_ratios = []

    for (_coord, nums) in potential_gears.items():
        if len(nums) == 2:
            gear_ratios.append(prod(nums))

    return (sum(nums_with_adjacent_symbol), sum(gear_ratios))

def is_symbol(char: str) -> bool:
    return char != '.' and not char.isdigit()

def adjacents(coords: tuple[int, int], max_bound: int) -> list[tuple[int, int]]:
    (x, y) = coords
    adj = []

    for i in range(x-1, (x+1)+1):
        for j in range(y-1, (y+1)+1):
            if (i >= 0 and j >= 0) and (i < max_bound and j < max_bound) and (i != x or j != y):
                adj.append((i, j))

    return adj

if __name__ == "__main__":
    main()
