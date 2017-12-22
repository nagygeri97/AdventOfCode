#include <iostream>
#include <map>
#include <utility>
#include <string>

#define MAPSIZE 25

using namespace std;

enum Dir{UP,RIGHT,DOWN,LEFT};
enum Node{CLEAN,WEAK,INF,FLAG};

void second(){
	map<pair<int,int>,Node> m;
	string s;
	for(int i = 0; i<MAPSIZE; ++i){
		cin >> s;
		for(int j = 0; j<MAPSIZE; ++j){
			if(s[j] == '#'){
				m[make_pair(i,j)] = INF;
			}
			else{
				m[make_pair(i,j)] = CLEAN;
			}
		}
	}
	pair<int,int> pos = pair<int,int>((MAPSIZE)/2,(MAPSIZE)/2);
	Dir d = UP; 
	int db = 0;
	//cout << pos.first << " " << pos.second << endl;
	for(int i = 0; i<10000000; ++i){
		//cout <<"dir: "<< d << " ";
		if(m[pos] == CLEAN){
			switch(d){
				case UP: d = LEFT; break;
				case RIGHT: d = UP; break;
				case DOWN: d = RIGHT; break;
				case LEFT: d = DOWN; break;
			}
			m[pos] = WEAK;
		}
		else if(m[pos] == INF){
			switch(d){
				case UP: d = RIGHT; break;
				case RIGHT: d = DOWN; break;
				case DOWN: d = LEFT; break;
				case LEFT: d = UP; break;
			}
			m[pos] = FLAG;
		}
		else if(m[pos] == FLAG){
			switch(d){
				case UP: d = DOWN; break;
				case RIGHT: d = LEFT; break;
				case DOWN: d = UP; break;
				case LEFT: d = RIGHT; break;
			}
			m[pos] = CLEAN;
		}
		else if(m[pos] == WEAK){
			++db;
			m[pos] = INF;
		}
		//cout << d << endl;
		switch(d){
			case UP: --pos.first; break;
			case DOWN: ++pos.first; break;
			case RIGHT: ++pos.second; break;
			case LEFT: --pos.second; break;
		}
		//cout << pos.first << " " << pos.second << endl; 
	}
	cout << db << endl;

}

void first(){
	map<pair<int,int>,bool> m;
	string s;
	for(int i = 0; i<MAPSIZE; ++i){
		cin >> s;
		for(int j = 0; j<MAPSIZE; ++j){
			if(s[j] == '#'){
				m[make_pair(i,j)] = true;
			}
			else{
				m[make_pair(i,j)] = false;
			}
		}
	}
	pair<int,int> pos = pair<int,int>((MAPSIZE)/2,(MAPSIZE)/2);
	Dir d = UP; 
	int db = 0;
	//cout << pos.first << " " << pos.second << endl;
	for(int i = 0; i<10000; ++i){
		//cout <<"dir: "<< d << " ";
		if(m[pos]){
			switch(d){
				case UP: d = RIGHT; break;
				case RIGHT: d = DOWN; break;
				case DOWN: d = LEFT; break;
				case LEFT: d = UP; break;
			}
			m[pos] = false;
		}
		else{
			switch(d){
				case UP: d = LEFT; break;
				case RIGHT: d = UP; break;
				case DOWN: d = RIGHT; break;
				case LEFT: d = DOWN; break;
			}
			++db;
			m[pos] = true;
		}
		//cout << d << endl;
		switch(d){
			case UP: --pos.first; break;
			case DOWN: ++pos.first; break;
			case RIGHT: ++pos.second; break;
			case LEFT: --pos.second; break;
		}
		//cout << pos.first << " " << pos.second << endl; 
	}
	cout << db << endl;

}

int main(){
	//first();
	second();
	return 0;
}