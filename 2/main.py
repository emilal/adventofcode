#!/usr/bin/env python

from sys import exit


def add(pos, array):
    array[array[pos+3]] = array[array[pos+1]] + array[array[pos+2]]
    return array


def mul(pos, array):
    array[array[pos+3]] = array[array[pos+1]] * array[array[pos+2]]
    return array


def halt(pos, array):
    return array[:1]


functions = {
    1: add,
    2: mul,
    99: halt,
    }


def run_opcode(pos, array):
    f = functions[array[pos]]
    return f(pos, array)


def runit(array):
    pos = 0
    while pos < len(array):
        try:
            array = run_opcode(pos, array)
            pos += 4
        except:
            print('whoops')
            return -1
    return array[0]


with open('input') as f:
    opcodes = list(map(int, f.read().split(',')))


# one
# fix state
old_opcodes = opcodes.copy()
old_opcodes[1] = 12
old_opcodes[2] = 2
print(runit(old_opcodes))

print("######################################################################")

# two
output = 0
op_arg1 = 0
op_arg2 = 0
wanted_num = 19690720
while output != wanted_num:
    my_opcodes = opcodes.copy()
    my_opcodes[1] = op_arg1
    my_opcodes[2] = op_arg2
    output = runit(my_opcodes)
    if output != wanted_num:
        if op_arg1 < 99:
            op_arg1 += 1
        else:
            if op_arg2 == 99:
                exit(1)
            else:
                op_arg1 = 0
                op_arg2 += 1
print(100 * op_arg1 + op_arg2)
