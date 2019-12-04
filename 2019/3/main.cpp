#include <iostream>
#include <utility>
#include <set>
#include <map>
#include <algorithm>
#include <sstream>
#include <cmath>

using namespace std;

map<pair<int,int>, int> populateMap(){
	string s;
	getline(cin, s);
	stringstream ss(s);
	int dist;
	int cx = 0, cy = 0;
	map<pair<int,int>, int> s1;
	int step = 0;
	while(ss >> s >> dist){
		if(s == "R"){
			for(int i = 0; i<dist; ++i){
				cx += 1;
				step += 1;
				s1[make_pair(cx,cy)] = step;
			}
		}
		if(s == "L"){
			for(int i = 0; i<dist; ++i){
				cx -= 1;
				step += 1;
				s1[make_pair(cx,cy)] = step;
			}
		}
		if(s == "U"){
			for(int i = 0; i<dist; ++i){
				cy += 1;
				step += 1;
				s1[make_pair(cx,cy)] = step;
			}
		}
		if(s == "D"){
			for(int i = 0; i<dist; ++i){
				cy -= 1;
				step += 1;
				s1[make_pair(cx,cy)] = step;
			}
		}
	}
	return s1;
}

int main(){
	auto s1 = populateMap();
	auto s2 = populateMap();
	map<pair<int,int>, int> res;
	set_intersection(s1.begin(), s1.end(), s2.begin(), s2.end(), inserter(res, res.begin()), [](pair<pair<int,int>, int> p1, pair<pair<int,int>, int>p2){return p1.first < p2.first;});
	int minDist = 99999999;
	int minSumDist = 99999999;
	for(const auto& p : res){
		if(abs(p.first.first) + abs(p.first.second) < minDist){
			minDist = abs(p.first.first) + abs(p.first.second);
		}
		if(s1[p.first] + s2[p.first] < minSumDist){
			minSumDist = s1[p.first] + s2[p.first];
		}
	}
	cout << minDist << "\n" << minSumDist << endl;
}