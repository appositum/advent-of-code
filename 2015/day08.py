from ast import literal_eval as unescape

def main():
    with open("inputs/day8.txt", "rb") as f:
        text = f.readlines()
        a, b = solve(text)

        print(f"part 1: {a}\npart 2: {b}")

def solve(b: bytes) -> tuple[int, int]:
    code_lengths = []
    strg_lengths = []
    encoded_code_lengths = []

    for code in b:
        code = code.strip()
        string = unescape(code.decode('utf8'))

        strg_lengths.append(len(string))
        code_lengths.append(len(code))

        encoded_code_lengths.append(2 + code.count(b'\\') + code.count(b'"'))

    return (sum(code_lengths) - sum(strg_lengths), sum(encoded_code_lengths))

if __name__ == "__main__":
    main()
