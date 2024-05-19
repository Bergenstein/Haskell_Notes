def binary_to_decimal(decimal : int) -> list[int]:
    if decimal == 0: return []
    res = []
    while decimal != 0:
        res.append(decimal % 2)
        decimal = int(decimal / 2) # decimal //= 2
    res.reverse()
    return res

def decimal_to_binary(binary : list[int]) -> int:
    return sum(b * (2 ** i) for i, b in enumerate(reversed(binary)))

def make_n(n: int, binary: list[int]) -> list[int]:
    res = binary[:]
    if n > len(binary):
        # Append trailing zeros
        res.extend([0] * (n - len(binary)))
    else:
        # Truncate to the first n elements
        res = binary[:n]
    
    return res
    
    return res
print(binary_to_decimal(13))
print(decimal_to_binary(binary_to_decimal(13)))
print(make_n(8, [1,0,1,1,0,0,1,1,1,1]))