from numpy import prod

input = '020D74FCE27E600A78020200DC298F1070401C8EF1F21A4D6394F9F48F4C1C00E3003500C74602F0080B1720298C400B7002540095003DC00F601B98806351003D004F66011148039450025C00B2007024717AFB5FBC11A7E73AF60F660094E5793A4E811C0123CECED79104ECED791380069D2522B96A53A81286B18263F75A300526246F60094A6651429ADB3B0068937BCF31A009ADB4C289C9C66526014CB33CB81CB3649B849911803B2EB1327F3CFC60094B01CBB4B80351E66E26B2DD0530070401C82D182080803D1C627C330004320C43789C40192D002F93566A9AFE5967372B378001F525DDDCF0C010A00D440010E84D10A2D0803D1761045C9EA9D9802FE00ACF1448844E9C30078723101912594FEE9C9A548D57A5B8B04012F6002092845284D3301A8951C8C008973D30046136001B705A79BD400B9ECCFD30E3004E62BD56B004E465D911C8CBB2258B06009D802C00087C628C71C4001088C113E27C6B10064C01E86F042181002131EE26C5D20043E34C798246009E80293F9E530052A4910A7E87240195CC7C6340129A967EF9352CFDF0802059210972C977094281007664E206CD57292201349AA4943554D91C9CCBADB80232C6927DE5E92D7A10463005A4657D4597002BC9AF51A24A54B7B33A73E2CE005CBFB3B4A30052801F69DB4B08F3B6961024AD4B43E6B319AA020020F15E4B46E40282CCDBF8CA56802600084C788CB088401A8911C20ECC436C2401CED0048325CC7A7F8CAA912AC72B7024007F24B1F789C0F9EC8810090D801AB8803D11E34C3B00043E27C6989B2C52A01348E24B53531291C4FF4884C9C2C10401B8C9D2D875A0072E6FB75E92AC205CA0154CE7398FB0053DAC3F43295519C9AE080250E657410600BC9EAD9CA56001BF3CEF07A5194C013E00542462332DA4295680'

def to_bin(hex_val):
    bin_val = bin(int(hex_val, 16))[2:]
    return ('0' * (4 * len(hex_val) - len(bin_val))) + bin_val

def get_version_type(bin_string):
    return (int(bin_string[:3], 2), int(bin_string[3:6], 2))

class Packet:
    sub_packets = []
    ops = {
        0: lambda p: sum(x.value() for x in p.sub_packets),
        1: lambda p: prod([x.value() for x in p.sub_packets]),
        2: lambda p: min(x.value() for x in p.sub_packets),
        3: lambda p: max(x.value() for x in p.sub_packets),
        4: lambda p: p.number,
        5: lambda p: 1 if p.sub_packets[0].value() > p.sub_packets[1].value() else 0,
        6: lambda p: 1 if p.sub_packets[0].value() < p.sub_packets[1].value() else 0,
        7: lambda p: 1 if p.sub_packets[0].value() == p.sub_packets[1].value() else 0,
    }

    def __init__(self, bin_val):
        self.bin_val = bin_val
        self.version, self.type = get_version_type(bin_val)
        self.length = len(bin_val)

    def version_total(self):
        return self.version + sum(p.version_total() for p in self.sub_packets)

    def value(self):
        return self.ops[self.type](self)

class Literal(Packet):
    def __init__(self, bin_val, number):
        super().__init__(bin_val)
        self.number = number

class Operator(Packet):
    def __init__(self, bin_val, sub_packets):
        super().__init__(bin_val)
        self.sub_packets = sub_packets

def get_literal(bin_string):
    num_part = bin_string[6:]
    num_bin = ''
    idx = 0
    while idx < len(num_part):
        num_bin += ''.join(num_part[idx+1:idx+5])
        if num_part[idx] == '0': break
        idx += 5

    return (Literal(bin_string[:6 + idx + 5], int(num_bin, 2)), bin_string[6 + idx + 5:])

def get_operator(bin_string):
    length_type = int(bin_string[6])
    sub_packets = []

    if length_type == 0:
        length = int(bin_string[7:22], 2)

        total_length = 0

        remaining = bin_string[22:]
        while total_length < length:
            sub_packet, remaining = get_head_packet(remaining)

            sub_packets.append(sub_packet)
            total_length += sub_packet.length
    else:
        length = int(bin_string[7:18], 2)
 
        remaining = bin_string[18:]
        while len(sub_packets) < length:
            sub_packet, remaining = get_head_packet(remaining)

            sub_packets.append(sub_packet)

    if len(remaining): bin_string = bin_string[:-len(remaining)]
    
    return (Operator(bin_string, sub_packets), remaining)

def get_head_packet(bin_string):
    _, type = get_version_type(bin_string)

    if type == 4: return get_literal(bin_string)
    else: return get_operator(bin_string)

packet, _ = get_head_packet(to_bin(input))

print(packet.version_total())
print(packet.value())