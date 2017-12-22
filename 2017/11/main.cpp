#include <iostream>
#include <string>
#include <cmath>

using namespace std;

void first(){
	string s;
	int u = 0;
	int e = 0;
	int w = 0;
	int m = 0;
	while(cin >> s){
		if(s == "n") {
			++u;

		}
		else if(s == "s") {
			--u;

		}
		else if(s == "ne") {
			--w;

		}
		else if(s == "sw") {
			++w;

		}
		else if(s == "nw") {
			--e;

		}
		else if(s == "se") {
			++e;

		}
		// cout << u << " " << w << " " << e << endl;
		int min = 0, max = 0;
		if(u < 0 && u < min) min = u;
		if(w < 0 && w < min) min = w;
		if(e < 0 && e < min) min = e;
		if(u > 0 && u > max) max = u;
		if(w > 0 && w > max) max = w;
		if(e > 0 && e > max) max = e;
		cout << abs(min) + max << endl;
		if(abs(min) + max > m) m = abs(min) + max;
		
	}
	int min = 0, max = 0;
	if(u < 0 && u < min) min = u;
	if(w < 0 && w < min) min = w;
	if(e < 0 && e < min) min = e;
	if(u > 0 && u > max) max = u;
	if(w > 0 && w > max) max = w;
	if(e > 0 && e > max) max = e;
	cout << abs(min) + max << endl;
	if(abs(min) + max > m) m = abs(min) + max;
	cout << m << endl;
}

int main(){
	first();
	return 0;
}