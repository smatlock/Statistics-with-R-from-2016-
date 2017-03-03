#!/usr/bin/python

# Shelby Matlock
# 04/20/2016 

import sys
CRM = sys.argv[1]
OUT = sys.argv[2]

with open(CRM, 'r') as file1:
	read_file = file1.read()
lines = read_file.splitlines()

output = open(OUT, 'w')

chromosome = lines[0]

# GC island dimer matrix from Durbin
sub_mat = [
[-0.740, .419, 0.580,-0.803], 	#AA, AC, AG, AT
[-0.913, 0.302, 1.812, -0.685], #CA, CC, CG, CT
[-0.624, 0.461, 0.331, -0.730],	#GA, GC, GG, GT
[-1.169, 0.573, 0.393, -0.679]]	#TA, TC, TG, TT

# makes ACGT correspond to sub_mat
look_up = {
"A":0,
"C":1,
"G":2,
"T":3}

def getScore(string):
	k = 0
	score = 0
	while k < len(string)-1:
		a = string[k]
		b = string[k+1]
		c = look_up[a]
		d = look_up[b]
		s = sub_mat[c][d]
		score += s
		k += 1
	return score



i = 0
j = 200
seq = []
pos = []
while i < len(chromosome):
	pos.append(i)
	seq.append(chromosome[i:j])
	i = i + 50
	j = j + 50

q = 0
output.write("pos:\t" + "score:\n")
while q < len(pos):
	if len(seq[q]) == 200:	# only want substrings of len(200)
		output.write(str(pos[q]) + "\t" + str(getScore(seq[q])) + "\n")
		getScore(seq[q])
		q += 1
	else:
		q += 1




