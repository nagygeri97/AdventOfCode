#include <iostream>
#include <vector>

using namespace std;

string original;
string s;
vector<bool> flags;


bool removeBasedOnShift(char c1, char c2){
	bool r = false;
	for(int i = 0; i<s.length()-1; ++i){
		if(s[i] - c1 == s[i+1] - c2){
			r = true;
			flags[i] = true;
			flags[i+1] = true;
			//cout << s[i] << s[i+1] << " removed, pos: " << i << endl; 
		}
	}
	string s2 = "";
	for(int i = 0; i<s.length(); ++i){
		if(!flags[i]) s2 += s[i];
		else flags[i] = false;
	}
	s = s2;
	return r;
}

string filterChar(char c){
	string s2 = "";
	for(char ch : original){
		if(ch == c || ch - 'A' + 'a' == c) continue;
		s2 += ch;
	}
	return s2;
}

int main(){
	ios::sync_with_stdio(false);
	cin >> original;
	s = original;
	flags.resize(s.length(),false);
	while(removeBasedOnShift('a','A') || removeBasedOnShift('A','a'));
	cout << s.length() << endl;

	int minl = s.length();
	for(char c = 'a'; c<='z'; ++c){
		s = filterChar(c);
		flags.clear();
		flags.resize(s.length(),false);
		while(removeBasedOnShift('a','A') || removeBasedOnShift('A','a'));
		if(s.length() < minl){
			minl = s.length();
		}
	}
	cout << minl << endl;
}