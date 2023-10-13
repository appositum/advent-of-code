from hashlib import md5

def main():
    with open('inputs/day04.txt', 'r') as f:
        cont = f.read().strip()

        print(f"part 1: {solve(cont, 5)}")
        print(f"part 2: {solve(cont, 6)}")

def solve(key: str, zeroes: int) -> int:
    number = 0

    while md5(bytes(key + str(number), 'utf8')).hexdigest()[0:zeroes] != zeroes * '0':
        number += 1

    return number

if __name__ == '__main__':
    main()
