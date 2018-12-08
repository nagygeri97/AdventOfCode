import pdb

def is_guard_line(line):
	return line.find('Guard') != -1

def get_id(guard_line):
	return int(guard_line.split(' ')[3][1:])

def get_minute(line):
	return int(line.split(' ')[1][3:-1])

with open('in.txt') as f:
	data = f.readlines()
data.sort()
guards = {}
fell = -1

for line in data:
	if is_guard_line(line):
		id = get_id(line)
		if id not in guards:
			guards[id] = [0] * 60
	elif fell == -1:
		fell = get_minute(line)
	else:
		woke = get_minute(line)
		for i in range(fell,woke):
			guards[id][i] += 1
		fell = -1

maxid = max([(sum(x[1]),x[0]) for x in guards.items()])[1]
maxmin = guards[maxid].index(max(guards[maxid]))

print(maxid * maxmin)

maxid2 = max([(max(x[1]),x[0]) for x in guards.items()])[1]
maxmin2 = guards[maxid2].index(max(guards[maxid2]))

print(maxid2 * maxmin2)

#pdb.set_trace()