#include <iostream>
#include <vector>
#include <utility>

using namespace std;

const int size = 97;

struct Scanner{
	int position;
	int maxpos;
	bool down;
	Scanner(int maxpos):position(0),maxpos(maxpos),down(true){}
	void step(){
		if(down){
			++position;
			if(position  == maxpos){
				position -= 2;
				down = false;
			}
		}
		else{
			--position;
			if(position == -1){
				position = 1;
				down = true;
			}
		}
	}
};

void first(){
	vector<Scanner*> scanners(size,nullptr);
	vector<pair<int,bool>> scannerStates(size,make_pair(0,true));
	int layer, maxpos;
	bool score = false;
	while(cin >> layer >> maxpos){
		scanners[layer] = new Scanner(maxpos);
	}
	int delay = 0;
	while(!score){
		for(int i = 0; i<size; ++i){
			if(scanners[i] != nullptr){
				scanners[i]->position = scannerStates[i].first;
				scanners[i]->down = scannerStates[i].second;
				scanners[i]->step();
				scannerStates[i].first = scanners[i]->position;
				scannerStates[i].second = scanners[i]->down;	
			}
		}
		for(Scanner* sc: scanners){
			if(sc != nullptr){sc->step();}
		}
		score = true;
		for(int currpos = 0; currpos < size; ++currpos){
			if(scanners[currpos] != nullptr){
				if(scanners[currpos]->position == 0){
					score = false;
					break;
				}
			}
			for(Scanner* sc: scanners){
				if(sc != nullptr){sc->step();}
			}
		}
		++delay;
		//cout << delay << endl;
	}
	cout << delay+1 << endl;
	for(Scanner* sc : scanners){
		delete sc;
	}
}

int main(){
	first();
	return 0;
}