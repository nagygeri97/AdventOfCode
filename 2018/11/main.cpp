#include <iostream>
#include <vector>
using namespace std;

int main(){
	int serialNum; cin >> serialNum;
	vector<vector<int>> powers(300,vector<int>(300,0));
	for(int x = 0; x<300; ++x){
		for(int y = 0; y<300; ++y){
			int rackID = x + 1 + 10;
			int start = rackID * (y + 1);
			int h = (start + serialNum) * rackID;
			int hundreds = (h/100) % 10;
			powers[x][y] = hundreds - 5;
		}
	}

	int maxPow = -10*300*300;
	int maxX = 0;
	int maxY = 0;
	int maxS = 0;
	for(int s = 1; s<=300; ++s){
		for(int i = 0; i<300-s+1; ++i){
			for(int j = 0; j<300-s+1; ++j){
				int power = 0;
				for(int a = 0; a<s; ++a){
					for(int b = 0; b<s; ++b){
						power += powers[i+a][j+b];
					}
				}
				if(power > maxPow){
					maxPow = power;
					maxX = i;
					maxY = j;
					maxS = s;
				}
			}
		}
	}
	cout << maxX + 1 << "," << maxY + 1 << "," << maxS << endl;
}