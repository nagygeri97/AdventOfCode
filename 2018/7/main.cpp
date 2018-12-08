#include <iostream>
#include <vector>

using namespace std;

struct Node{
	int needed_before;
	bool done;
	vector<bool> is_needed;
	Node():needed_before(0), done(false), is_needed(26,false){}
};

vector<Node> v;

int main(){
	char a,b;
	v.resize(26);
	while(cin >> a >> b){
		v[b-'A'].needed_before += 1;
		v[b-'A'].is_needed[a-'A'] = true;
	}
	vector<int> freeAt(5,-1);
	vector<int> worksOn(5,'.' - 'A');
	int currTime = 0;
	int done = 0;
	while(done < 26){
		for(int i = 0; i<5; ++i){
			if(freeAt[i] == currTime){
				int id = worksOn[i];
				worksOn[i] = '.' - 'A';
				++done;
				for(Node& n : v){
					if(n.is_needed[id]){
						n.needed_before -= 1;
						n.is_needed[id] = false;
					}
				}
			}
		}
		for(int i = 0; i<26; ++i){
			if(!v[i].done && v[i].needed_before == 0){
				for(int j = 0; j<5; ++j){
					if(freeAt[j] <= currTime){
						freeAt[j] = currTime + 60 + i + 1;
						worksOn[j] = i;
						v[i].done = true;
						break;
					}
				}
			}
		}
		cout << currTime << "\t";
		for(int i = 0; i<5; ++i) cout << char(worksOn[i] + 'A') << "\t";
		cout << endl;
		++currTime;
	}
	cout << endl;
	cout << currTime - 1<< endl;
}