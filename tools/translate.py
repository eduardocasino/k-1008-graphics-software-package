#!/usr/bin/env python 

import fileinput
import re

print('        .CASE   -')

print('        .ORG    0')

for l in fileinput.input():

    #if None != re.search("^[^;]{0,}.PAGE[ \t\']|^[ \t]{0,};", l):
    #    continue
    
    l = re.sub("^([ \t]{0,}.PAGE[ \t\'].*)", ";\g<1>", l)
    l = re.sub('X\'', '$', l)
    l = re.sub('\.[ \t]{0,}=[ \t]{0,}\.[ \t]{0,}\+', '.RES', l)
    l = re.sub('\.[ \t]{0,}=[ \t]{0,}([0-9A-Z$]{1,})(.*)', '.RES    \g<1>-*\g<2>', l)
    l = re.sub('([^A-Z0-9])(ASL|ROL|LSR|ROR)A([^A-Z0-9])', '\g<1>\g<2> A\g<3>', l)
    #l = re.sub('([^A-Z0-9])(LDA|STA)([ \t]{0,}[^,]{1,},[ \t]{0,}Y[ \t;]{0,}.*)$', '\g<1>U \g<2>,\g<3>', l)
    print(l, end='')
