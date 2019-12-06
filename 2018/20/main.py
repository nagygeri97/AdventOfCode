regex = raw_input('')
regex = regex[1:-1]
dim = 500
x = dim/2
y = dim/2
it = 0
distances = []
for i in range(dim):
	dist = []
	for j in range(dim):
		dist.append(-1)
	distances.append(dist)
distances[x][y] = 0

def findEndParen(regex):
	openCount = 1
	choices = []
	start = 0
	i = 0
	while openCount > 0:
		if regex[i] == '(':
			openCount += 1
		elif regex[i] == ')':
			openCount -= 1
		elif regex[i] == '|' and openCount == 1:
			choices.append(regex[start:i])
			start = i+1
		i += 1
	choices.append(regex[start:i-1])
	return (choices,i-1)

def walk(regex):
	global x
	global y
	global distances
	global it
	for i in range(len(regex)):
		it += 1
		if it % 1000 == 0:
			print it
		c = regex[i]
		if c == '(':
			regex = regex[i+1:]
			choices, endParen = findEndParen(regex)
			regex = regex[endParen+1:]
			cx = x
			cy = y
			for eachChoice in choices:
				x = cx
				y = cy
				walk(eachChoice + regex)
			return
		else:
			d = distances[x][y]
			if c == 'N':
				x += 1
			elif c == 'S':
				x -= 1
			elif c == 'E':
				y += 1
			elif c == 'W':
				y -= 1
			if distances[x][y] == -1:
				distances[x][y] = d + 1
			elif distances[x][y] > d + 1:
				distances[x][y] = d + 1


walk(regex)

maxDoorCount = max([max(d) for d in distances])
# print distances
print maxDoorCount