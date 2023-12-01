from collections import defaultdict
from math import ceil

test_input = '''NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C'''
input = '''CVKKFSSNNHNPSPPKBHPB

OF -> S
VO -> F
BP -> S
FC -> S
PN -> K
HC -> P
PP -> N
FK -> V
KN -> C
BO -> O
KS -> B
FF -> S
KC -> B
FV -> C
VF -> N
HS -> H
OS -> F
VC -> S
VP -> P
BC -> O
HF -> F
HO -> F
PC -> B
CC -> K
NB -> N
KK -> N
KP -> V
BH -> H
BF -> O
OB -> F
VK -> P
FB -> O
NP -> B
CB -> C
PS -> S
KO -> V
SP -> C
BK -> O
NN -> O
OC -> F
VB -> B
ON -> K
NK -> B
CK -> H
NH -> N
CV -> C
PF -> P
PV -> V
CP -> N
FP -> N
SB -> B
SN -> N
KF -> F
HP -> S
BN -> V
NF -> B
PO -> O
CH -> O
VV -> S
OV -> V
SF -> P
BV -> S
FH -> V
CN -> H
VH -> V
HB -> B
FN -> P
OH -> S
SK -> H
OP -> H
VN -> V
HN -> P
BS -> S
CF -> B
PB -> H
SS -> K
NV -> P
FS -> N
CS -> O
OK -> B
CO -> O
VS -> F
OO -> B
NO -> H
SO -> F
HH -> K
FO -> H
SH -> O
HV -> B
SV -> N
PH -> F
BB -> P
KV -> B
KB -> H
KH -> N
NC -> P
SC -> S
PK -> B
NS -> V
HK -> B'''

template, rule_list = input.split('\n\n')
rules = { line.split(' -> ')[0]: line.split(' -> ')[1] for line in rule_list.split('\n') }

def print_answer(pair_counts):
    elem_counts = defaultdict(lambda: 0)
    for p in pair_counts:
        elem_counts[p[0]] += pair_counts[p]
        elem_counts[p[1]] += pair_counts[p]

    most_common = ceil(max(elem_counts.values()) / 2.)
    least_common = ceil(min(elem_counts.values()) / 2.)
    print(int(most_common - least_common))

pairs = [''.join(template[i:i+2]) for i,_ in enumerate(template[:-1])]
pair_counts = { p: pairs.count(p) for p in set(pairs) }

for c in range(40):
    new_pair_counts = defaultdict(lambda: 0)
    for p in pair_counts:
        if p in rules:
            new_pair_counts[p[0] + rules[p]] += pair_counts[p]
            new_pair_counts[rules[p] + p[1]] += pair_counts[p]
        else:
            new_pair_counts[p] += pair_counts[p]

    pair_counts = new_pair_counts

    if c == 9: print_answer(pair_counts)

print_answer(pair_counts)