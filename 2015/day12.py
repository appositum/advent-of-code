import json

def main():
    with open("inputs/day12.txt") as f:
        j = json.load(f)
        print(f"part 1: {part1(j)}\npart 2: {part2(j)}")

def part1(collection):
    total = 0

    lst = collection.values() if type(collection) == dict else collection

    for i in filter(lambda x: type(x) != str, lst):
        if type(i) == list:
            total += part1(i)
        elif type(i) == dict:
            total += part1(filter(lambda x: type(x) != str, i.values()))
        else:
            total += i

    return total

def part2(collection):
    total = 0

    lst = collection.values() if type(collection) == dict else collection

    for i in filter(lambda x: type(x) != str, lst):
        if type(i) == list:
            total += part2(i)
        elif type(i) == dict:
            if 'red' not in i.values():
                total += part2(filter(lambda x: type(x) != str, i.values()))
        else:
            total += i

    return total

if __name__ == "__main__":
    main()
