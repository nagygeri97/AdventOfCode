#include <iostream>
#include <vector>
#include <queue>
#include <memory>
#include <set>

#define INFINITY 10000000
#define START_HITPOINTS 200
#define START_ATTACKPOINTS 3
#define START_ELF_ATTACKPOINTS 3

using namespace std;

int n,m;

enum TileType{Wall,Path};
enum UnitType{Elf,Goblin};

struct Point{
	int x;
	int y;

	Point():x(0),y(0){}

	Point(int x_, int y_):
		x(x_), y(y_)	
	{}

	Point above(){
		return Point(x-1,y);
	}
	Point below(){
		return Point(x+1,y);
	}
	Point right(){
		return Point(x,y-1);
	}
	Point left(){
		return Point(x,y+1);
	}
	bool isValid(){
		return x >= 0 && y >= 0 && x < n && y < m;
	}
	vector<Point> getNeighboursInReadingOrder(){
		vector<Point> neighbours;
		if(above().isValid()) neighbours.push_back(above());
		if(left().isValid()) neighbours.push_back(left());
		if(right().isValid()) neighbours.push_back(right());
		if(below().isValid()) neighbours.push_back(below());
		return neighbours;
	}
};

bool operator<(const Point& lhs, const Point& rhs) 
{
   if(lhs.x == rhs.x) return lhs.y < rhs.y;
   else return lhs.x < rhs.x;
}
bool operator>(const Point& lhs, const Point& rhs){return rhs < lhs;}
bool operator==(const Point& lhs, const Point& rhs){return lhs.x == rhs.x && lhs.y == rhs.y;}
bool operator<=(const Point& lhs, const Point& rhs){return !(lhs > rhs);}
bool operator>=(const Point& lhs, const Point& rhs){return !(lhs < rhs);}
ostream& operator<<(ostream& os, const Point& p){return (os << p.x << " " << p.y);}

struct Unit{
	UnitType type;
	Point coordinates;
	int hitPoints;
	int attackPoints;

	Unit(){}

	Unit(UnitType type_, Point coordinates_):
		type(type_), coordinates(coordinates_), hitPoints(START_HITPOINTS), attackPoints(START_ATTACKPOINTS)
	{}

	Unit(UnitType type_, int x_, int y_):
		type(type_), coordinates(x_, y_), hitPoints(START_HITPOINTS), attackPoints(START_ATTACKPOINTS)
	{}

	Unit(UnitType type_, Point coordinates_, int attackPoints_):
		type(type_), coordinates(coordinates_), hitPoints(START_HITPOINTS), attackPoints(attackPoints_)
	{}

	Unit(UnitType type_, int x_, int y_, int attackPoints_):
		type(type_), coordinates(x_, y_), hitPoints(START_HITPOINTS), attackPoints(attackPoints_)
	{}

	bool isAlive() const {return hitPoints > 0;}
};	

typedef shared_ptr<Unit> UnitPtr;

struct Tile{
	TileType type;
	Point coordinates;	
	UnitPtr unitPtr;

	Tile(){}

	Tile(TileType type_, Point coordinates_):
		type(type_), coordinates(coordinates_), unitPtr(nullptr)
	{}

	Tile(TileType type_, int x_, int y_):
		type(type_), coordinates(x_, y_), unitPtr(nullptr)
	{}

	Tile(TileType type_, Point coordinates_, UnitPtr unitPtr_):
		type(type_), coordinates(coordinates_), unitPtr(unitPtr_)
	{}

	Tile(TileType type_, int x_, int y_, UnitPtr unitPtr_):
		type(type_), coordinates(x_, y_), unitPtr(unitPtr_)
	{}
};

typedef vector<Tile> Row;
typedef vector<Row> Field;

Field field;
vector<UnitPtr> elfs;
vector<UnitPtr> goblins;
vector<UnitPtr> units;
int elfAttackPoints = START_ELF_ATTACKPOINTS;
bool elfDied = false;

queue<UnitPtr> stepOrder; 

vector<string> raw_input;

void parseInput(){
	int x = 0, y = 0;
	for(string s : raw_input){
		y = 0;
		Row row;
		for(auto c : s){
			Tile tile;
			UnitPtr unitPtr;
			switch(c){
				case '#':{
					tile = Tile(Wall, x, y, nullptr);
					break;
				}
				case '.':{
					tile = Tile(Path, x, y, nullptr);
					break;
				}
				case 'E':{
					unitPtr = make_shared<Unit>(Elf, x, y, elfAttackPoints);
					tile = Tile(Path, x, y, unitPtr);
					elfs.push_back(unitPtr);
					units.push_back(unitPtr);
					break;
				}
				case 'G':{
					unitPtr = make_shared<Unit>(Goblin, x, y);
					tile = Tile(Path, x, y, unitPtr);
					goblins.push_back(unitPtr);
					units.push_back(unitPtr);
					break;
				}
			}
			row.push_back(tile);
			++y;
		}
		field.push_back(row);
		++x;
	}
}

void read(){
	string s;
	while(getline(cin,s)){
		raw_input.push_back(s);
	}
	n = raw_input.size();
	m = raw_input[0].length();
}

void reset(){
	field.clear();
	elfs.clear();
	goblins.clear();
	units.clear();
	elfDied = false;
	parseInput();
}

void print(){
	for(const Row& row : field){
		for(const Tile& tile : row){
			if(tile.unitPtr == nullptr){
				if(tile.type == Wall) cout << '#';
				else cout << '.';
			}
			else{
				if(tile.unitPtr->type == Elf) cout << 'E';
				else cout << 'G';
			}
		}
		cout << endl;
	}
	cout << endl;
}

void populateQueue(){
	for(const Row& row : field){
		for(const Tile& tile : row){
			if(tile.unitPtr != nullptr){
				stepOrder.push(tile.unitPtr);
			}
		}
	}
}

vector<Point> getTargets(UnitType type){
	vector<Point> targets;
	if(type == Elf){
		for(UnitPtr goblin : goblins){
			if(goblin->isAlive()) targets.push_back(goblin->coordinates);
		}
	}
	else{
		for(UnitPtr elf : elfs){
			if(elf->isAlive()) targets.push_back(elf->coordinates);
		}
	}
	return targets;
}

set<Point> getInRange(vector<Point>& targets){
	set<Point> inRange;
	for(Point& p : targets){
		vector<Point> neighbours = p.getNeighboursInReadingOrder();
		for(Point& np : neighbours){
			inRange.insert(np);
		}
	}
	return inRange;
}

set<Point> getReachable(Point position, set<Point>& inRange){
	vector<vector<bool>> reachable(n,vector<bool>(m,false));
	reachable[position.x][position.y] = true;
	queue<Point> q;
	q.push(position);
	while(!q.empty()){
		Point p = q.front();
		q.pop();
		vector<Point> ps = p.getNeighboursInReadingOrder();
		for(Point& pt : ps){
			if(field[pt.x][pt.y].type == Path && field[pt.x][pt.y].unitPtr == nullptr && !reachable[pt.x][pt.y]){
				reachable[pt.x][pt.y] = true;
				q.push(pt);
			}
		}
	}
	set<Point> reachablePts;
	for(Point p : inRange){
		if(reachable[p.x][p.y]){
			reachablePts.insert(p);
		}
	}
	return reachablePts;
}

int getDistanceAndNextPointOnPath(Point a, Point b, Point& next){
	if(a == b){
		next = a;
		return 0;
	}
	vector<vector<int>> distances(n,vector<int>(m,INFINITY));
	distances[b.x][b.y] = 0;
	queue<Point> q;
	q.push(b);
	while(!q.empty()){
		Point p = q.front();
		q.pop();
		vector<Point> ps = p.getNeighboursInReadingOrder();
		for(Point& pt : ps){
			if(field[pt.x][pt.y].type == Path && field[pt.x][pt.y].unitPtr == nullptr){
				if(distances[pt.x][pt.y] > distances[p.x][p.y] + 1){
					distances[pt.x][pt.y] = distances[p.x][p.y] + 1;
					q.push(pt);
				}
			}
			else if(pt == a){
				if(distances[pt.x][pt.y] > distances[p.x][p.y] + 1){
					distances[pt.x][pt.y] = distances[p.x][p.y] + 1;
				}
			}
		}
	}

	vector<Point> neighbours = a.getNeighboursInReadingOrder();
	int i = 0;
	while(distances[neighbours[i].x][neighbours[i].y] != distances[a.x][a.y] - 1) ++i;
	next = neighbours[i];
	return distances[a.x][a.y];
}

bool doMove(UnitPtr unitPtr){
	vector<Point> targets = getTargets(unitPtr->type);
	if(targets.size() == 0) return false;

	set<Point> inRange = getInRange(targets);
	set<Point> reachable = getReachable(unitPtr->coordinates, inRange);

	int minDistance = INFINITY;
	Point minPoint = Point(n+1,m+1);
	Point nextPoint;
	for(Point p : reachable){
		Point next;
		int dist = getDistanceAndNextPointOnPath(unitPtr->coordinates,p,next);
		if(dist < minDistance){
			minDistance = dist;
			minPoint = p;
			nextPoint = next;
		}
		else if(dist == minDistance && p < minPoint){
			minPoint = p;
			nextPoint = next;
		}
	}

	if(minDistance == INFINITY || nextPoint == unitPtr->coordinates){
		//Do nothing, no move
	}
	else{
		//Move to nextPoint
		field[unitPtr->coordinates.x][unitPtr->coordinates.y].unitPtr = nullptr;
		unitPtr->coordinates = nextPoint;
		field[unitPtr->coordinates.x][unitPtr->coordinates.y].unitPtr = unitPtr;
	}

	return true;
}

UnitType oppositeUnitType(UnitType type){
	if(type == Elf) return Goblin;
	else return Elf;
}

void attack(UnitPtr unitPtr){
	vector<Point> neighbours = unitPtr->coordinates.getNeighboursInReadingOrder();
	UnitPtr targetPtr = nullptr;
	int minHP = START_HITPOINTS + 1;
	for(Point& p: neighbours){
		UnitPtr targetUnitPtr = field[p.x][p.y].unitPtr;
		if(targetUnitPtr != nullptr){
			if(targetUnitPtr->type == oppositeUnitType(unitPtr->type)){
				if(targetUnitPtr->hitPoints > 0 && targetUnitPtr->hitPoints < minHP){
					minHP = targetUnitPtr->hitPoints;
					targetPtr = targetUnitPtr;
				}
			}
		}
	}
	if(targetPtr == nullptr){
		//Do not attack;
	}
	else{
		targetPtr->hitPoints -= unitPtr->attackPoints;
		if(targetPtr->hitPoints <= 0){
			field[targetPtr->coordinates.x][targetPtr->coordinates.y].unitPtr = nullptr;
			if(targetPtr->type == Elf) elfDied = true;
		}
	}
}

bool step(UnitPtr unitPtr){
	if(unitPtr->isAlive()){
		if(!doMove(unitPtr)) return false;
		attack(unitPtr);
	}
	return true;
}

bool simulateRound(){
	populateQueue();
	while(!stepOrder.empty()){
		UnitPtr currentUnitPtr = stepOrder.front();
		stepOrder.pop();
		if(!step(currentUnitPtr)) return false;
	}
	return true;
}

int main(){
	read();
	reset();
	uint64_t roundCount;

	while(roundCount < 1000 && simulateRound()){
		// print();
		++roundCount;
	}
	cout << "Part 1: " << endl;
	cout << "rounds: " << roundCount << endl;
	uint64_t sumHP = 0;
	for(UnitPtr unitPtr : units){
		if(unitPtr->isAlive()){
			sumHP += unitPtr->hitPoints;
		}
	}
	cout << "sum of hps: " << sumHP << endl;
	uint64_t score = sumHP * roundCount;
	cout << "score: " << score << endl;

	do{
		reset();
		roundCount = 0;
		// cout << elfAttackPoints << endl;
		while(roundCount < 1000 && simulateRound() && !elfDied){
			// print();
			++roundCount;
		}
		++elfAttackPoints;
	} while(elfDied);

	cout << endl << "Part 2: " << endl;
	cout << "attack points needed: " << elfAttackPoints-1 << endl;

	cout << "rounds: " << roundCount << endl;
	sumHP = 0;
	for(UnitPtr unitPtr : units){
		if(unitPtr->isAlive()){
			sumHP += unitPtr->hitPoints;
		}
	}
	cout << "sum of hps: " << sumHP << endl;
	score = sumHP * roundCount;
	cout << "score: " << score << endl;
}