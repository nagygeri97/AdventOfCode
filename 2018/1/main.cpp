#include <iostream>
#include <map>
#include <vector>
using namespace std;

int main(){
	map<int,int> m;
	int n, s = 0, s2;
	bool b = false;
	vector<int> v;
	m[0] = 1;
	while(cin >> n){
		s += n;
		v.push_back(n);
	}
	cout << s << endl;
	s = 0;
	while(!b){
		for(int i = 0; i<v.size(); ++i){
			s += v[i];
			if(m[s] == 1){
				b = true;
				break;
			}
			m[s] = 1;
		}
	}
	cout << s << endl;
}