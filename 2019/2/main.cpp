#include <iostream>
#include <vector>

using namespace std;

int run(vector<int> v){
	int i = 0;
	while(v[i] != 99){
		if(v[i] == 1){
			v[v[i + 3]] = v[v[i + 1]] + v[v[i + 2]];
		}
		else if(v[i] == 2){
			v[v[i + 3]] = v[v[i + 1]] * v[v[i + 2]];
		}
		i += 4;
	}
	return v[0];
}

int main(){
	vector<int> v;
	int s;
	while(cin >> s){
		v.push_back(s);
	}
	// Part 1
	v[1] = 12; v[2] = 2;
	cout << run(v) << endl;
	// Part 2
	for(int i = 0; i<100; ++i){
		for(int j = 0; j<100; ++j){
			v[1] = i; v[2] = j;
			if(run(v) == 19690720){
				cout << 100*i + j << endl;
				return 0;
			}
		}
	}
}