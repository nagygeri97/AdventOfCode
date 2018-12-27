#include <iostream>
#include <vector>
#include <queue>

using namespace std;

enum Type{Clay, Sand};
enum Water{None, Flowing, Still, Source};

struct Tile{
	Type type;
	Water water;
	Tile(): type(Sand), water(None) {}
};

enum Direction{Vertical, Horizontal};
struct InputLine{
	Direction dir;
	int x;
	int yBegin;
	int yEnd;
};

vector<vector<Tile>> tiles;
vector<InputLine> inputs;
int maxX = 0;
int maxY = 0;
int minY = 0;
int minX = 1000;
int xOffset;
int lowerBoundY = 99999;
int upperBoundY = 0;

queue<pair<int,int>> canFlow;

void read(){
	char c;
	while(cin >> c)
	{
		InputLine line;
		if(c == 'x'){
			line.dir = Vertical;
		}
		else{
			line.dir = Horizontal;
		}
		cin >> line.x >> c >> line.yBegin >> line.yEnd;
		if(line.dir == Vertical){
			if(line.x > maxX){
				maxX = line.x;
			}
			if(line.x < minX){
				minX = line.x;
			}
			if(line.yEnd > maxY){
				maxY = line.yEnd;
			}
			if(line.yEnd > upperBoundY){
				upperBoundY = line.yEnd;
			}
			if(line.yBegin < lowerBoundY){
				lowerBoundY = line.yBegin;
			}
		}
		else{
			if(line.x > upperBoundY){
				upperBoundY = line.yEnd;
			}
			if(line.x > maxY){
				maxY = line.yEnd;
			}
			if(line.x < lowerBoundY){
				lowerBoundY = line.yBegin;
			}
			if(line.yEnd > maxX){
				maxX = line.yEnd;
			}
			if(line.yBegin < minX){
				minX = line.yBegin;
			}
		}
		inputs.push_back(line);
	}
	tiles.resize(maxX-minX+3,vector<Tile>(maxY+1));
	xOffset = minX - 1;

	for(InputLine line : inputs){
		if(line.dir == Vertical){
			int x = line.x;
			for(int y = line.yBegin; y<=line.yEnd; ++y){
				tiles[x-xOffset][y].type = Clay;
			}
		}
		else{
			int y = line.x;
			for(int x = line.yBegin; x<=line.yEnd; ++x){
				tiles[x-xOffset][y].type = Clay;
			}
		}
	}
	
	tiles[500-xOffset][0].water = Source;
}

void print(){
	for(int y = 0; y<tiles[0].size(); ++y){
		for(int x = 0; x<tiles.size(); ++x){
			if(tiles[x][y].water == Still) cout << '~';
			else if(tiles[x][y].water == Source) cout << '+';
			else if(tiles[x][y].water == Flowing) cout << '|';
			else if(tiles[x][y].type == Sand) cout << '.';
			else if(tiles[x][y].type == Clay) cout << '#';
		}
		cout << endl;
	}
	cout << endl;
}

void makeStill(){
	int start = -1;
	int length = 0;
	for(int y = 0; y<tiles[0].size(); ++y){
		for(int x = 0; x<tiles.size(); ++x){
			if(tiles[x][y].type == Clay){
				if(length > 0){
					for(int i = start+1; i<x; ++i){
						tiles[i][y].water = Still;
						if(y-1 >= 0 && tiles[i][y-1].water == Flowing){
							canFlow.emplace(i,y-1);
						}
					}
				}
				start = x;
				length = 0;
			}
			else if(tiles[x][y].water == Flowing && start != -1){
				++length;
			}
			else{
				start = -1;
				length = 0;
			}
		}
	}
}

void move(){
	canFlow.emplace(500-xOffset,0);
	canFlow.emplace(-1,-1);

	while(!canFlow.empty()){
		// print();
		auto p = canFlow.front();
		int x = p.first; int y = p.second;
		canFlow.pop();
		if(x == -1 && y == -1){
			makeStill();
			if(canFlow.size() > 0) canFlow.emplace(-1,-1);
		}
		else{
			if(y+1 < tiles[0].size()){
				if(tiles[x][y+1].type == Clay || tiles[x][y+1].water == Still){ //Move left and right
					//Left
					if(x-1 >= 0 && tiles[x-1][y].type == Sand && tiles[x-1][y].water == None){
						tiles[x-1][y].water = Flowing;
						canFlow.emplace(x-1,y);
					}
					//Right
					if(x+1 < tiles.size() && tiles[x+1][y].type == Sand && tiles[x+1][y].water == None){
						tiles[x+1][y].water = Flowing;
						canFlow.emplace(x+1,y);
					}
				}
				else{ //Move down
					if(tiles[x][y+1].type == Sand && tiles[x][y+1].water == None){
						tiles[x][y+1].water = Flowing;
						canFlow.emplace(x,y+1);
					}
				}
			}
		}
	}
}

int countWater(){
	int count = 0;
	for(int y = lowerBoundY; y<=upperBoundY; ++y){
		for(int x = 0; x<tiles.size(); ++x){
			if(tiles[x][y].water != None) ++count;
		}
	}
	return count;
}

int countStill(){
	int count = 0;
	for(int y = lowerBoundY; y<=upperBoundY; ++y){
		for(int x = 0; x<tiles.size(); ++x){
			if(tiles[x][y].water == Still) ++count;
		}
	}
	return count;
}

int main(){
	read();
	// print();
	move();
	print();
	cout << "Part 1: " << countWater() << endl;
	cout << "Part 2: " << countStill() << endl;
}