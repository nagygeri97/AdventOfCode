#include <iostream>
#include <vector>
#include <string>

using namespace std;

enum Direction{UP,DOWN,LEFT,RIGHT};

void first(){
	vector<vector<char>> v;
	v.resize(201,vector<char>(201));
	string s;
	int j = 0;
	while(getline(cin,s)){
		for(int i = 0; i<s.size(); ++i){
			v[j][i] = s[i];
		}
		++j;
	}
	Direction d = DOWN;
	int posx = 0;
	int step = 0;
	int posy = 1;

	while(posx >= 0 && posy >= 0 && posx < 201 && posy < 201){
		//if(v[posx][posy] - 'A' >= 0 && v[posx][posy] - 'Z' <= 0) cout << v[posx][posy];
		++step;
		if(v[posx][posy] == 'S') {cout << step << endl; break;}
		switch(d){
			case DOWN:{
				++posx;
				if(posx < 201 && v[posx][posy] == '+'){
					if(posy > 0 && v[posx][posy-1] == '-') d = LEFT;
					else d = RIGHT;
				}
				break;
			}
			case UP:{
				--posx;
				if(posx >= 0 && v[posx][posy] == '+'){
					if(posy > 0 && v[posx][posy-1] == '-') d = LEFT;
					else d = RIGHT;
				}
				break;
			}
			case RIGHT:{
				++posy;
				if(posy < 201 && v[posx][posy] == '+'){
					if(posx > 0 && v[posx-1][posy] == '|') d = UP;
					else d = DOWN;
				}
				break;
			}
			case LEFT:{
				--posy;
				if(posy >= 0 && v[posx][posy] == '+'){
					if(posx > 0 && v[posx-1][posy] == '|') d = UP;
					else d = DOWN;
				}
				break;
			}
		}
	}
	// cout << endl;

}

int main(){
	first();
	return 0;
}