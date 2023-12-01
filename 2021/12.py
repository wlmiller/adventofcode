from collections import defaultdict

test_input1 = '''start-A
start-b
A-c
A-b
b-d
A-end
b-end'''
test_input2 = '''dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc'''
test_input3 = '''fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW'''
input = '''hl-WP
vl-fo
vl-WW
WP-start
vl-QW
fo-wy
WW-dz
dz-hl
fo-end
VH-fo
ps-vl
FN-dz
WP-ps
ps-start
WW-hl
end-QW
start-vl
WP-fo
end-FN
hl-QW
WP-dz
QW-fo
QW-dz
ps-dz'''

graph = defaultdict(lambda: [])
for line in input.split('\n'):
    x, y = line.split('-')
    graph[x].append(y)
    graph[y].append(x)

def paths_to_end(frm, graph, max_visit_count = 1, exclude = []):
    if frm == 'end': return [[frm]]
    elif not len(graph[frm]): return [[]]

    if frm.islower():
        exclude = exclude + [frm]

    paths = []
    for to in [x for x in graph[frm] if x != 'start']:
        if to not in exclude or max(exclude.count(x) for x in exclude) < max_visit_count:
            paths += [[frm] + p for p in paths_to_end(to, graph, max_visit_count, exclude)]
            
    return [p for p in paths if p[-1] == 'end']

print(len(paths_to_end('start', graph)))
print(len(paths_to_end('start', graph, 2)))