from math import floor, ceil

test_input = '''[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]'''
input = '''[4,[3,[9,[9,0]]]]
[[[7,6],[2,[2,5]]],[5,[[7,3],8]]]
[4,[4,6]]
[[0,[5,6]],[[[1,3],[2,7]],[[0,6],4]]]
[6,[[3,[6,0]],3]]
[[7,[9,[8,5]]],[6,7]]
[[[[2,6],1],2],[3,[8,4]]]
[4,[[[5,4],[2,7]],[[8,0],[2,3]]]]
[[[[4,3],2],[[3,6],[2,5]]],[[[3,7],8],0]]
[[[8,[0,7]],1],[[9,[3,9]],9]]
[[[[3,0],[1,3]],[[0,9],8]],[[[7,2],9],[[1,4],[3,5]]]]
[[[[9,6],[4,4]],[1,3]],[[4,3],[[6,4],[8,4]]]]
[[[1,2],[[7,6],[2,3]]],[[4,6],[4,2]]]
[[[4,8],[[5,8],1]],[2,3]]
[[[5,2],[3,[5,7]]],[[2,9],5]]
[[[6,[3,2]],[2,6]],[[8,[4,2]],[[5,2],7]]]
[[[[2,6],[0,1]],[7,[3,6]]],[[1,6],[[7,9],0]]]
[[[0,3],[8,1]],[[[9,0],3],[0,2]]]
[[8,[[7,1],[4,7]]],[[0,[1,3]],[8,2]]]
[[[[2,3],4],[[0,8],[9,0]]],[1,[[5,3],4]]]
[[[[7,2],2],[[1,3],[8,3]]],[4,[[7,9],[0,6]]]]
[[[[2,2],[3,4]],[[1,5],[4,3]]],[6,[[7,2],1]]]
[1,[[[5,7],0],[9,[8,8]]]]
[[[[9,2],[0,9]],[4,[7,8]]],[[4,8],[[1,8],[4,9]]]]
[[[[4,7],2],2],4]
[1,[[2,[4,2]],1]]
[[[[7,2],[3,8]],[0,[1,3]]],[[[4,4],[2,4]],[8,2]]]
[[[[1,0],[0,5]],2],[[9,[5,0]],[[1,6],5]]]
[4,[[[8,1],[1,4]],[7,[1,3]]]]
[[[6,[0,4]],[[4,6],[2,4]]],[9,[1,5]]]
[[[[3,6],[3,3]],1],[0,[[8,8],2]]]
[[7,[5,[2,6]]],[[[7,9],6],[0,[3,6]]]]
[[[[6,7],4],[[2,9],2]],3]
[[[7,[1,7]],[5,4]],[[[1,1],[0,1]],5]]
[[6,[[1,0],6]],[0,[6,[0,5]]]]
[[[[2,4],[4,6]],9],[4,[[8,0],7]]]
[[[[9,9],[5,7]],[9,[8,6]]],[[3,[2,3]],0]]
[[0,[1,[5,3]]],[3,[8,[3,4]]]]
[[[[4,3],8],[2,9]],[[1,[6,5]],[[5,7],2]]]
[[[0,[7,4]],[9,[9,6]]],[[8,[5,5]],[[6,4],1]]]
[[[[7,3],[7,9]],[8,[6,2]]],[[8,[4,5]],[[6,4],[6,7]]]]
[[7,[[9,0],[9,0]]],[[[0,8],2],[8,[8,3]]]]
[4,[7,[5,6]]]
[7,[[[3,8],8],3]]
[[[4,[6,6]],0],[9,0]]
[[[[7,4],8],8],[[0,1],[[0,0],[2,4]]]]
[7,[1,[[9,4],[3,6]]]]
[[[[2,8],9],[[8,6],[2,2]]],[[[5,1],9],[2,[0,7]]]]
[8,7]
[[[[0,8],4],[[9,9],[9,9]]],[[[4,3],[1,0]],[6,8]]]
[[[[8,3],[8,9]],1],[[4,[1,0]],[[4,0],[2,3]]]]
[[[[4,7],[1,3]],[6,9]],[[1,0],[[1,8],5]]]
[[2,[4,[6,5]]],[3,[[9,9],5]]]
[[[[7,6],4],9],[8,[4,5]]]
[[[0,[6,6]],[7,[8,9]]],[[[0,0],[3,4]],[4,[1,8]]]]
[[[9,[7,0]],[5,8]],[6,[[5,0],[0,6]]]]
[[[[4,0],[1,9]],[7,[3,6]]],[[2,[8,6]],[[2,8],[8,2]]]]
[[[9,6],8],[[[5,5],[4,8]],0]]
[[[[1,7],1],2],[[[6,8],3],[[3,3],5]]]
[3,[5,[[3,8],6]]]
[3,[[[9,6],[5,8]],[9,2]]]
[[6,1],[6,4]]
[[2,6],[[[1,2],2],8]]
[[[[1,7],[3,6]],[2,[0,2]]],[[3,0],9]]
[1,[[0,[4,9]],5]]
[[[[5,5],[5,2]],[0,[6,4]]],8]
[0,[7,[[6,9],[6,0]]]]
[[[[2,2],[4,7]],[[7,4],6]],[[0,[1,7]],[[3,2],6]]]
[[9,8],0]
[[[[5,4],[4,8]],2],[3,[8,9]]]
[[[[7,0],8],5],[2,6]]
[[[5,[0,8]],5],[[[5,0],[1,8]],[[0,2],7]]]
[[[[9,4],8],[[6,5],4]],[[5,[8,9]],[4,[0,4]]]]
[[[[3,6],7],[[9,3],7]],[7,[[8,3],9]]]
[[[[0,7],5],[[5,7],2]],[[2,[9,5]],[[7,7],[5,0]]]]
[[[[7,5],2],[8,6]],[[2,[6,2]],[5,[3,1]]]]
[[9,[9,1]],6]
[[[0,7],[[5,9],2]],3]
[[[9,3],[8,8]],[0,[4,5]]]
[[[[6,2],5],[4,[3,1]]],[9,[2,8]]]
[[[1,[9,4]],[[0,0],2]],[[1,[2,1]],[[7,8],[3,2]]]]
[[[[0,6],[8,9]],[[4,7],[5,6]]],[[[1,4],[8,7]],[4,6]]]
[[[[6,4],[1,5]],[0,8]],[[[9,7],[1,2]],[9,4]]]
[[[[4,5],[0,7]],[9,[1,8]]],[[[5,0],6],7]]
[[[0,[6,9]],[5,[5,6]]],7]
[[4,5],[[7,[6,5]],1]]
[[[7,9],[6,7]],[4,1]]
[[[[9,6],1],[[3,1],[9,7]]],[1,[7,1]]]
[[[0,[2,0]],5],[[8,[7,6]],[[7,3],4]]]
[[[6,[1,7]],[9,[2,7]]],3]
[[[6,[8,2]],5],[4,[[1,3],[5,1]]]]
[[[4,[3,3]],[4,[2,4]]],[5,4]]
[[[1,6],[4,[4,0]]],[[8,[2,2]],[[8,1],[4,7]]]]
[[2,0],[[2,1],[[4,8],[2,7]]]]
[9,[[8,4],0]]
[[1,6],[[5,[1,3]],[9,[0,9]]]]
[[[0,[3,5]],3],[[2,[8,0]],[[2,0],[4,3]]]]
[[[1,[1,9]],[9,[7,9]]],[[2,2],[[6,7],[0,7]]]]
[[[4,6],[[6,2],[0,9]]],[[1,0],[1,[6,7]]]]
[9,[[[0,1],4],[[9,3],3]]]'''

class Number:
    left = None
    right = None
    parent = None
    def __init__(self, left, right):
        self.left = left
        self.left.parent = self
        self.left.inc_depth()

        self.right = right
        self.right.parent = self
        self.right.inc_depth()
        
        self.depth = 0

    def __str__(self):
        return f'[{self.left},{self.right}]'

    def get_left_regular(self):
        num = self
        while num.parent and num == num.parent.left:
            num = num.parent

        if not num or not num.parent: return None
        
        num = num.parent.left

        while num.right: num = num.right

        return num

    def get_right_regular(self):
        num = self
        while num.parent and num == num.parent.right:
            num = num.parent

        if not num or not num.parent: return None
        
        num = num.parent.right

        while num.left: num = num.left

        return num

    def replace(self, num):
        num.parent = self.parent
        num.depth = self.parent.depth + 1

        if self == self.parent.left: self.parent.left = num
        if self == self.parent.right: self.parent.right = num

    def inc_depth(self):
        self.depth += 1
        self.left.inc_depth()
        self.right.inc_depth()

    def explode(self):
        if self.depth == 4:
            left = self.get_left_regular()
            right = self.get_right_regular()

            if left: left.val += self.left.val
            if right: right.val += self.right.val

            self.replace(RegularNumber(0))        
            return True
        
        return self.left.explode() or self.right.explode()

    def split(self):
        return self.left.split() or self.right.split()

    def reduce(self):
        reduced = True
        while reduced:
            reduced = self.explode() or self.split()

        return self

    def plus(self, num):
        return Number(self, num).reduce()

    def magnitude(self):
        return 3 * self.left.magnitude() + 2 * self.right.magnitude()

    @classmethod
    def parse(cls, str):
        if str.isnumeric(): return RegularNumber(int(str))

        d = 0
        stripped = str[1:-1]
        for i,c in enumerate(stripped):
            if c == '[': d += 1
            elif c == ']': d -= 1

            if d == 0 and c == ',':
                left, right = stripped[:i], stripped[i+1:]
                return Number(Number.parse(left), Number.parse(right))

class RegularNumber(Number):
    def __init__(self, val):
        self.val = val
        self.depth = 0

    def __str__(self): return str(self.val)

    def inc_depth(self): self.depth += 1

    def explode(self): return False

    def split(self):
        if self.val >= 10:
            new_left, new_right = floor(self.val / 2), ceil(self.val / 2)
            
            self.replace(Number(RegularNumber(new_left), RegularNumber(new_right)))
            return True
        else: return False

    def magnitude(self): return self.val

lines = input.split('\n')
vals = [Number.parse(l) for l in lines]

sum = vals[0]
for v in vals[1:]:
    sum = sum.plus(v)

print(sum.magnitude())

max_mag = 0
for i,_ in enumerate(lines):
    for j,_ in enumerate(lines):
        if i == j: continue

        max_mag = max(max_mag, Number.parse(lines[i]).plus(Number.parse(lines[j])).magnitude())

print(max_mag)