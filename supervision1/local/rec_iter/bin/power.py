def power(x, n):
    out = 1
    while n > 0:
        if n % 2:
            out *= x
            n -= 1
        else:
            x *= x
            n //= 2
    return out

print(power(2, 7))