test_input = 'target area: x=20..30, y=-10..-5'
input = 'target area: x=175..227, y=-134..-79'

target_area = tuple(tuple(int(x.replace(',','')) for x in coord.split('=')[1].split('..')) for coord in input.split(' ')[2:])

def run_sim(dx, dy, target_area):
    x, y = (0, 0)
    path = [(0, 0)]
    while True:
        x, y = (x + dx, y + dy)
        path.append((x, y))
        dx  = max(dx - 1, 0)
        dy -= 1

        if x < target_area[0][0] and dx == 0: return (path, False)
        elif x > target_area[0][1]: return (path, False)
        elif y < target_area[1][0]: return (path, False)
        elif x >= target_area[0][0] and x <= target_area[0][1] and y >= target_area[1][0] and y <= target_area[1][1]:
            return (path, True)

count = 0
max_height = 0
for dx in range(1, target_area[0][1] + 1):
    for dy in range(target_area[1][0], -target_area[1][0] + 1):
        path, succ = run_sim(dx, dy, target_area)

        if succ:
            count += 1

            height = max(path, key = lambda p: p[1])[1]
            max_height = max(height, max_height)

print(max_height)
print(count)