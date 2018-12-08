#include <vector>
#include <iostream>
#include <cmath>

using namespace std;

struct Point{
	int x,y,c;
	Point():x(0),y(0),c(0){}
	Point(int x, int y):x(x),y(y),c(0){}
};

int main(){
	int a,b;
	vector<Point> ps;
	while(cin >> a >> b){
		ps.emplace_back(a,b);
	}
	int d;
	int mind;
	int minind;
	int s;
	int db = 0;
	for(int i = -500; i < 1000; ++i){
		for(int j = -500; j < 1000; ++j){
			mind = 5000;
			minind = -1;
			s = 0;
			for(int k = 0; k<ps.size(); ++k){
				d = abs(ps[k].x - i) + abs(ps[k].y - j);
				s += d;
				if(d < mind){
					mind = d;
					minind = k;
				}
				else if(d == mind){
					minind = -1;
				}
			}
			if(minind >= 0) ps[minind].c += 1;
			if(s < 10000) ++db;
		}
	}
	int max = 0;
	int maxi = -1;
	for(int i = 0; i<ps.size(); ++i){
		if(ps[i].c > max && ps[i].c < 5000){
			max = ps[i].c;
			maxi = i;
		}
		//cout << i << " " << ps[i].c << endl;
	}
	cout << maxi << " " << max << endl;
	cout << db << endl;
}