#include <iostream>
#include <vector>
#include <list>

using namespace std;

void first(){
	list<int> buff = {0};
	int steps;
	auto it = buff.begin();
	cin >> steps;
	for(int i = 1; i<=50000000; ++i){
		for(int j = 0; j<steps; ++j){
			++it;
			if(it == buff.end()){
				it = buff.begin();
			}
		}
		++it;
		it = buff.insert(it,i);
	}
	cout << *(++buff.begin()) << endl;
}

int main(){
	first();
	return 0;
}