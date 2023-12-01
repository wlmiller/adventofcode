test_input = '''Player 1 starting position: 4
Player 2 starting position: 8'''
input = '''Player 1 starting position: 1
Player 2 starting position: 6'''

lines = input.split('\n')
pos = [int(l.split(' ')[-1]) for l in lines]
scores = [0, 0]

turn = 0
die = 1
rolls = 0
while scores[0] < 1000 and scores[1] < 1000:
    pos[turn] += 3 * (die + 1)
    while pos[turn] > 10: pos[turn] -= 10
    scores[turn] += pos[turn]

    die += 3
    if die > 100: die -= 100
    rolls += 3
    turn = 1 if turn == 0 else 0

print(min(scores) * rolls)

memo = {}
def play(pos, scores, turn = None, roll = None):
    key = (pos, scores, turn, roll)

    if key in memo: return memo[key]

    if roll:
        new_pos = pos[turn] + roll
        if new_pos > 10: new_pos -= 10
        
        pos = tuple(new_pos if i == turn else p for i,p in enumerate(pos))
        scores = tuple(s + new_pos if i == turn else s for i,s in enumerate(scores))

        if scores[turn] >= 21: return tuple(1 if s >= 21 else 0 for s in scores)

    turn = 1 if turn == 0 else 0

    next_rolls = [x + y + z for x in [1, 2, 3] for y in [1, 2, 3] for z in [1, 2, 3]]

    win_counts = [play(pos, scores, turn, r) for r in next_rolls]

    val = tuple(sum(c[i] for c in win_counts) for i,_ in enumerate(win_counts[0]))

    memo[key] = val

    return val

pos = tuple(int(l.split(' ')[-1]) for l in lines)

print(max(play(pos, (0, 0))))