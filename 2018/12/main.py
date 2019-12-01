import sys

pots = ""
rules = {}

for line in sys.stdin:
	line = line.rstrip()
	if pots == "":
		pots = line
	else:
		s = line.split(' ')
		rules[s[0]] = s[1]

zeroOffset = 100
pots = zeroOffset*'.' + pots + zeroOffset*'.'

for i in range(500):
	s = "."
	for j in range(2,len(pots)-2):
		relevants = pots[(j-2):(j+3)]
		s = s + rules[relevants]
	pots = s + "..."
	zeroOffset -= 1
	# print(pots)

score = 0
zeroOffset += 500 - 50000000000
for i in range(len(pots)):
	if pots[i] == "#":
		score += i-zeroOffset
print(score)