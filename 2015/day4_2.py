from hashlib import md5

def solve(key: str) -> int:
    number = 0

    while md5(bytes(key + str(number), 'utf8')).hexdigest()[0:6] != '000000':
        number += 1

    return number

print(solve("ckczppom"))
