import sys

currPos = 0
circle = [0]

for line in sys.stdin:
	nums = line.rstrip().split(' ')
	numPlayers = int(nums[0])
	numMarbles = int(nums[1])
	break

currPlayer = 0
playerScores = [0 for i in range(numPlayers)]

for currMarble in range(1,numMarbles + 1):
	if currMarble % 23 == 0:
		playerScores[currPlayer] += currMarble
		currPos -= 7
		if currPos < 0:
			currPos += len(circle)
		playerScores[currPlayer] += circle.pop(currPos)
		if currPos == len(circle):
			currPos = 0
	else:
		currPos = currPos + 2
		if currPos > len(circle):
			currPos -= len(circle)
		circle.insert(currPos,currMarble)
	# print(str(currPlayer) + " " + str(circle))
	currPlayer = (currPlayer + 1) % numPlayers

maxScore = max(playerScores)
maxScoreId = playerScores.index(maxScore) + 1
print(str(maxScoreId) + " has the most score with " + str(maxScore) + " pts")
