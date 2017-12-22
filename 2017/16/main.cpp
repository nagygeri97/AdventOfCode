#include <iostream>
#include <string>
#include <fstream>
#include <vector>

using namespace std;

const int size = 16;

char p[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

void swap(int i, int j){
	char x = p[i];
	p[i] = p[j];
	p[j] = x;
}

void first(){
	ifstream is("in.txt");
	string s;
	while(is >> s){
		if(s[0] == 's'){
			int n;
			is >> n;
			vector<char> end;
			for(int i =  size-n; i< size; ++i){
				end.push_back(p[i]);
			}
			for(int i = 0; i< size-n; ++i){
				end.push_back(p[i]);
			}
			
			for(int i = 0; i< size; ++i){
				p[i] = end[i];
			}
		}
		else if(s[0] == 'x'){
			int a;
			int b;
			is >> a >> b;
			swap(a,b);
		}
		else if(s[0] == 'p'){
			char ac, bc;
			is >> ac >> bc;
			int a = ac - 'a';
			int b = bc - 'a';
			int ai, bi;
			for(int i = 0; i< size; ++i){
				if(p[i] == a){
					ai = i;
				}
				if(p[i] == b){
					bi = i;
				}
			}
			swap(ai,bi);
		}
	}
	is.close();
	for(int i = 0; i< size; ++i){
		cout << (char)(p[i] + 'a');
	}
	cout << endl;
}

char q[16];  

void second(){
	cout << 1000000000%63 << endl;
	for(int i = 0; i<100; ++i){
		first();
	}
}

int main(){
	second();
	return 0;
}