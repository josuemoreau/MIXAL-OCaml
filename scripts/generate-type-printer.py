text = """nop
add sub mul div
num char hlt
sla sra slax srax slc src
move
lda ld1 ld2 ld3 ld4 ld5 ld6 ldx
ldan ld1n ld2n ld3n ld4n ld5n ld6n ldxn
sta st1 st2 st3 st4 st5 st6 stx stj stz
jbus ioc in out jred
jmp jsj jov jnov jl je jg jge jne jle
jan jaz jap jann janz janp
j1n j1z j1p j1nn j1nz j1np
j2n j2z j2p j2nn j2nz j2np
j3n j3z j3p j3nn j3nz j3np
j4n j4z j4p j4nn j4nz j4np
j5n j5z j5p j5nn j5nz j5np
j6n j6z j6p j6nn j6nz j6np
jxn jxz jxp jxnn jxnz jxnp
inca deca enta enna
inc1 dec1 ent1 enn1
inc2 dec2 ent2 enn2
inc3 dec3 ent3 enn3
inc4 dec4 ent4 enn4
inc5 dec5 ent5 enn5
inc6 dec6 ent6 enn6
incx decx entx ennx
cmpa cmp1 cmp2 cmp3 cmp4 cmp5 cmp6 cmpx
"""

type_name = "mixop"

output_str_type = "let {0}_of_str = function\n".format(type_name)
output_type_str = "let str_of_{0} = function\n".format(type_name)

type_def = "type {0} =\n".format(type_name)

for line in text.splitlines():
    for word in line.split(' '):
        type_def += " | {0}".format(word.upper())
        output_str_type += "| \"{0}\" -> {1}\n".format(word, word.upper())
        output_type_str += "| {0} -> \"{1}\"\n".format(word.upper(), word)
    type_def += "\n"

print("str to type : " + output_str_type + "| _ -> assert (false)")
print("type to str : " + output_type_str)
print("type definition : " + type_def)
