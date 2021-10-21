def countPoints(a: list[int], b: list[int]) -> int:
    assert(len(a) == len(b))
    count: int = 0
    for i, element in enumerate(a):
        if a[i] > b[i]:
            count += 1
    return count

def compareTriplets(a: list[int], b: list[int]) -> list[int]:
    return [countPoints(a, b), countPoints(b,a)]
