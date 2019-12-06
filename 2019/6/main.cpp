#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

using namespace std;

struct Node{
	Node(int id): id(id), score(0), hasParent(false), parent(-1) {}
	int id;
	vector<int> children;
	int score;
	bool hasParent;
	int parent;
};

int calcScore(vector<Node>& nodes, int id){
	int res = nodes[id].score;
	for(auto& childId : nodes[id].children){
		nodes[childId].score = nodes[id].score + 1;
		res += calcScore(nodes, childId);
	}
	return res;
}

vector<int> parents(vector<Node>& nodes, int id){
	vector<int> pars;
	while(id != -1){
		pars.push_back(nodes[id].parent);
		id = nodes[id].parent;
	}
	reverse(pars.begin(), pars.end());
	return pars;
}

int main(){
	map<string, int> nameIdMap;
	vector<Node> nodes;
	string parent, child;
	while(cin >> parent >> child){
		if(nameIdMap.find(parent) == nameIdMap.end()){
			nodes.emplace_back(nodes.size());
			nameIdMap[parent] = nodes.size() - 1;
		}
		if(nameIdMap.find(child) == nameIdMap.end()){
			nodes.emplace_back(nodes.size());
			nameIdMap[child] = nodes.size() - 1;
		}
		int parentId = nameIdMap[parent];
		int childId = nameIdMap[child];
		nodes[parentId].children.push_back(childId);
		nodes[childId].hasParent = true;
		nodes[childId].parent = parentId;
	}
	int rootId = 0;
	while(nodes[rootId].hasParent) ++rootId;
	cout << calcScore(nodes, rootId) << endl;

	int youId = nameIdMap["YOU"];
	int sanId = nameIdMap["SAN"];
	auto youP = parents(nodes, youId);
	auto sanP = parents(nodes, sanId);
	int i = 0;
	while(i < youP.size() && i < sanP.size() && youP[i] == sanP[i]) ++i;
	cout << youP.size() - i + sanP.size() - i << endl;

}