test_input = '''5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526'''
input = '''4781623888
1784156114
3265645122
4371551414
3377154886
7882314455
6421348681
7175424287
5488242184
2448568261'''

vals = [[int(x) for x in row] for row in input.split('\n')]

def flash(x, y, vals):
    for y2 in range(max(0, y - 1), min(y + 2, len(vals))):
        for x2 in range(max(0, x - 1), min(x + 2, len(vals[y]))):
            vals[y2][x2] += 1

idx = 0
count = 0
while True:
    vals = [[x + 1 for x in row] for row in vals]

    flashes = []
    while any(val > 9 and not (x, y) in flashes for y, row in enumerate(vals) for x, val in enumerate(row)):
        for y, row in enumerate(vals):
            for x, val in enumerate(row):
                if val > 9 and not (x, y) in flashes:
                    flashes.append((x, y))
                    flash(x, y, vals)
                    count += 1
    
    for x, y in flashes:
        vals[y][x] = 0

    idx += 1
    if (idx == 100): print(count)

    if all(x == 0 for row in vals for x in row):
        print(idx)
        break