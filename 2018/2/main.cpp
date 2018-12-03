#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

int diff(string& a, string& b){
	int d = -1;
	for(int i = 0; i<a.length(); ++i){
		if(a[i] ^ b[i]){
			if(d < 0) d = i;
			else return -1;
		}
	}
	return d;
}

int main(){
	string s;
	vector<int> counts;
	vector<string> strs;
	int twos = 0;
	int threes = 0;
	while(cin >> s){
		strs.push_back(s);
		counts.clear();
		counts.resize('z'-'a'+1, 0);
		for(auto& x : s){
			++counts[x - 'a'];
		}
		if(find(counts.begin(), counts.end(), 2) != counts.end()) ++twos;
		if(find(counts.begin(), counts.end(), 3) != counts.end()) ++threes;
	}
	cout << twos * threes << endl;

	for(auto& x : strs){
		for(auto& y : strs){
			int d = diff(x,y);
			if(d >= 0){
				for(int i = 0; i<y.length(); ++i){
					if(i == d) continue;
					cout << y[i];
				}
				cout << endl;
				return 0;
			}
		}
	}
}
