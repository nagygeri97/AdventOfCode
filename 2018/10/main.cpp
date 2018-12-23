#include <iostream>
#include <vector>

using namespace std;

struct Point{
	int x,y;
	int vx,vy;
	Point(int x, int y, int vx, int vy):x(x), y(y), vx(vx), vy(vy){}
};

void kiir(vector<Point>& v, int minx, int maxx, int miny, int maxy){
	uint64_t meret = uint64_t(maxx-minx)*uint64_t(maxy-miny);
	if(meret > 100000){
		cout << "meret: " << meret << endl;
		cout << "minx: " << minx << endl;
		cout << "maxx: " << maxx << endl;
		cout << "miny: " << miny << endl;
		cout << "maxy: " << maxy << endl;
		return;
	}
	vector<vector<char>> kep((maxy-miny+1),vector<char>((maxx-minx+1),'.'));
	for(Point& p : v){
		kep[p.y - miny][p.x - minx] = '#';
	}

	for(int i = 0; i<kep.size(); ++i){
		for(int j = 0; j<kep[i].size(); ++j){
			cout << kep[i][j];
		}
		cout << endl;
	}
}

int main(){
	ios_base::sync_with_stdio(false);
	vector<Point> v;
	int x,y,vx,vy;
	while(cin >> x >> y >> vx >> vy){
		v.emplace_back(x,y,vx,vy);
	}
	vector<Point> voriginal = v;
	uint64_t minarea = 1000000000000000000;
	int miniter = 0;
	int i = 1;
	while(i < 100000){
		int minx = 1000000, maxx = -1000000, miny = 1000000, maxy = -1000000;
		for(Point& p : v){
			p.x += p.vx;
			p.y += p.vy;
			if(p.x < minx) minx = p.x;
			if(p.x > maxx) maxx = p.x;
			if(p.y < miny) miny = p.y;
			if(p.y > maxy) maxy = p.y;
		}
		uint64_t area = (maxx-minx)*(maxy-miny);
		if(area <= minarea){
			minarea = area;
			miniter = i;
		}
		++i;
	}
	cout << "MINAREA = " << minarea << endl;
	cout << "miniter = " << miniter << endl;
	int minx = 1000000, maxx = -1000000, miny = 1000000, maxy = -1000000;
	for(Point& p : voriginal){
		p.x += miniter*p.vx;
		p.y += miniter*p.vy;
		if(p.x < minx) minx = p.x;
		if(p.x > maxx) maxx = p.x;
		if(p.y < miny) miny = p.y;
		if(p.y > maxy) maxy = p.y;
	}
	kiir(voriginal,minx,maxx,miny,maxy);

}