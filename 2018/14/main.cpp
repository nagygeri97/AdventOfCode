#include <iostream>
#include <vector>

using namespace std;

bool good(vector<int>& v, vector<int>& s){
	for(int i = 0; i<s.size(); ++i){
		if(v[v.size()-1-i] == s[i]){
			continue;
		}
		return false;
	}
	return true;
}

int main(){
	string n; cin >> n;
	vector<int> v = {3,7};
	v.reserve(100000000);
	int a = 0;
	int b = 1;
	vector<int> k;
	for(int i = 0; i<n.length(); ++i){
		k.push_back(n[n.length()-1-i] - '0');
	}
	while(v.size() < k.size() || !good(v,k)){
		// for(int i = 0; i<v.size(); ++i){
		// 	if(i == a && i == b){
		// 		cout << '{' << v[i] << '}';
		// 	}
		// 	else if(i == a){
		// 		cout << '(' << v[i] << ')';
		// 	}
		// 	else if(i == b){
		// 		cout << '[' << v[i] << ']';
		// 	}
		// 	else{
		// 		cout << ' ' << v[i] << ' ';
		// 	}
		// }
		// cout << endl;
		int r = v[a] + v[b];
		if(r >= 10){
			int s = r%10;
			r = r/10;
			v.push_back(r);
			if(good(v,k)){
				break;
			}
			v.push_back(s);
		}
		else{
			v.push_back(r);
		}
		a += 1 + v[a];
		b += 1 + v[b];
		a %= v.size();
		b %= v.size();
		if(v.size() > 100000000) break;
	}

	// for(int i = 0; i<10; ++i){
	// 	cout << v[n+i];
	// }
	// cout << endl;

	cout << v.size() - k.size() << endl;
}