from collections import Counter

def main():
    with open("inputs/day02.txt", "r") as f:
        lines = f.readlines()
        part1 = checksum(lines)
        part2 = common(lines)

        print(f"part 1: {part1}\npart 2: {part2}")

def checksum(ids: list[str]):
    two_times = 0
    three_times = 0
    counts = map(lambda line: set(Counter(line.strip()).values()), ids)

    for count in counts:
        for n in count:
            if n == 2:
                two_times += 1

            if n == 3:
                three_times += 1

    return two_times * three_times

def common(ids: list[str]):
    ids = list(map(lambda x: x.strip(), ids))

    for id1 in ids:
        for id2 in ids:
            if id1 != id2:
                difference = set(id1).difference(set(id2))

                if len(difference) == 1:
                    return "".join(filter(lambda c: c not in difference, id1))

if __name__ == "__main__":
    main()
