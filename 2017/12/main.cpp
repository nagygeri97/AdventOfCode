#include <iostream>
#include <vector>
#include <queue>
#include <sstream>

using namespace std;

const int size = 2000;

struct Node{
	int id;
	bool visited;
	vector<Node*> neighbours;
};

void first(){
	vector<Node> nodes(size);
	for(int i = 0; i<size; ++i){nodes[i].id = i; nodes[i].visited = false;}
	string s;
	for(int i = 0; i<size; ++i){
		getline(cin,s);
		stringstream ss(s);
		int id;
		int curr;
		ss >> curr;
		while(ss >> id){
			nodes[curr].neighbours.push_back(&nodes[id]);
		}
	}
	int count = 0;
	for(int i = 0; i< size; ++i){
		if(!nodes[i].visited){
			++count;
			queue<Node*> q;
			q.push(&nodes[i]);
			while(!q.empty()){
				Node* n = q.front();
				q.pop();
				if(!n->visited){
					n->visited = true;
					for(Node* node : n->neighbours){
						q.push(node);
					}
				}
			}
		}
	}
	cout << count << endl;
}

int main(){
	first();
	return 0;
}