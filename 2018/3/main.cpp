#include <iostream>
#include <vector>

using namespace std;

struct Request{
	int id, left, top, width, height;
};

int main(){
	vector<Request> reqs;
	
	int id, left, top, width, height;
	int maxw = 0, maxh = 0;
	int count = 0;
	while(cin >> id >> left >> top >> width >> height){
		reqs.push_back(Request{id, left, top, width, height});
		if(left + width > maxw) maxw = left + width;
		if(top + height > maxh) maxh = top + height;		
	}
	vector<vector<int>> v(maxw,vector<int>(maxh,0));
	for(auto& r : reqs){
		for(int i = r.left; i < r.left + r.width; ++i){
			for(int j = r.top; j < r.top + r.height; ++j){
				if(v[i][j] == 1) ++count;
				++v[i][j];
				
			}
		}
	}
	cout << count << endl;
	for(auto& r : reqs){
		bool good = true;
		for(int i = r.left; good && i < r.left + r.width; ++i){
			for(int j = r.top; good && j < r.top + r.height; ++j){
				if(v[i][j] != 1)good = false;
			}
		}
		if(good){cout << r.id << endl; break;}
	}
}