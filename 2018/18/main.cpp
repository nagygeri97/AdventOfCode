#include <iostream>
#include <vector>

using namespace std;

struct TileData{
	int tree;
	int lumber;
	int open;
	TileData(): tree(0), lumber(0), open(0) {}
	TileData(int tree, int lumber, int open): tree(tree), lumber(lumber), open(open) {}

	void add(char c){
		if(c == '.') ++open;
		else if(c == '#') ++lumber;
		else if(c == '|') ++tree;
	}
};

vector<vector<char>> field;
vector<vector<vector<char>>> fields;

void read(){
	string line;
	while(cin >> line){
		vector<char> lineVector;
		for(char c : line) lineVector.push_back(c);
		field.push_back(lineVector);
	}
	fields.push_back(field);
}

void print1(){
	for(auto& line : fields[10]){
		for(auto& c : line){
			cout << (char)c;
		}
		cout << endl;
	}
	cout << endl;
}

void print(){
	for(auto& line : field){
		for(auto& c : line){
			cout << (char)c;
		}
		cout << endl;
	}
	cout << endl;
}

char getNext(char c, TileData td){
	if(c == '.'){
		if(td.tree >= 3) return '|';
		else return '.';
	}
	else if(c == '|'){
		if(td.lumber >= 3) return '#';
		else return '|';
	}
	else if(c == '#'){
		if(td.tree >= 1 && td.lumber >= 1) return '#';
		else return '.';
	}
}

TileData getTileData(int x, int y){
	TileData td;
	if(x-1 >= 0){
		if(y-1 >= 0) td.add(field[x-1][y-1]);
		td.add(field[x-1][y]);
		if(y+1 < field[0].size()) td.add(field[x-1][y+1]);
	}
	if(y-1 >= 0) td.add(field[x][y-1]);
	if(y+1 < field[0].size()) td.add(field[x][y+1]);
	if(x+1 < field.size()){
		if(y-1 >= 0) td.add(field[x+1][y-1]);
		td.add(field[x+1][y]);
		if(y+1 < field[0].size()) td.add(field[x+1][y+1]);
	}
	return td;
}

int findRepetition(){
	for(int i = 0; i<fields.size(); ++i){
		if(fields[i] == field) return i;
	}
	return -1;
}

bool step(){
	vector<vector<TileData>> tileData(field.size(),vector<TileData>(field[0].size()));
	for(int i = 0; i<field.size(); ++i){
		for(int j = 0; j<field[0].size(); ++j){
			tileData[i][j] = getTileData(i,j);
		}
	}

	for(int i = 0; i<field.size(); ++i){
		for(int j = 0; j<field[0].size(); ++j){
			field[i][j] = getNext(field[i][j],tileData[i][j]);
		}
	}
	int rep = findRepetition();
	if(rep == -1){
		fields.push_back(field);
		return true;
	}
	else{
		cout << rep << " " << fields.size() << endl;
		int repSize = fields.size() - rep;
		int repStart = rep;
		field = fields[(1000000000 - repStart) % repSize + repStart];
		return false;
	}
}

TileData countTotal1(){
	TileData td;
	for(int i = 0; i<fields[10].size(); ++i){
		for(int j = 0; j<fields[10][0].size(); ++j){
			td.add(fields[10][i][j]);
		}
	}
	return td;
}

TileData countTotal(){
	TileData td;
	for(int i = 0; i<field.size(); ++i){
		for(int j = 0; j<field[0].size(); ++j){
			td.add(field[i][j]);
		}
	}
	return td;
}

int main(){
	read();
	for(int i = 0; i<1000; ++i){
		if(!step()) break;
	}
	print1();
	TileData td = countTotal1();
	cout << "Part 1: " << endl;
	cout << "tree: " << td.tree << endl;
	cout << "lumber: " << td.lumber << endl;
	cout << "open: " << td.open << endl;
	cout << "result: " << td.tree * td.lumber << endl << endl;

	print();
	td = countTotal();
	cout << "Part 2: " << endl;
	cout << "tree: " << td.tree << endl;
	cout << "lumber: " << td.lumber << endl;
	cout << "open: " << td.open << endl;
	cout << "result: " << td.tree * td.lumber << endl;
}