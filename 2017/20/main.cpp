#include <iostream>
#include <cmath>
#include <vector>
#include <set>

using namespace std;

typedef signed long long int SInt64;

struct Particle{
	struct Triplet{
		SInt64 x; SInt64 y; SInt64 z;
		Triplet():x(0),y(0),z(0){}
		Triplet(SInt64 x, SInt64 y, SInt64 z):x(x),y(y),z(z){}
		
	};
	struct Pos : public Triplet{
		Pos(SInt64 x, SInt64 y, SInt64 z):Triplet(x,y,z), used(false){} 
		Pos():Triplet(), used(false){}
		bool used;
	};
	struct Vel : public Triplet{Vel(SInt64 x, SInt64 y, SInt64 z):Triplet(x,y,z){} Vel():Triplet(){}};
	struct Acc : public Triplet{Acc(SInt64 x, SInt64 y, SInt64 z):Triplet(x,y,z){} Acc():Triplet(){}};
	Pos p;
	Vel v;
	Acc a;
	bool collided;
	void simulate();
	SInt64 dist() const;

	Particle():collided(false){}
};

Particle::Triplet operator+(const Particle::Triplet& lhs, const Particle::Triplet& rhs){
		return Particle::Triplet(lhs.x + rhs.x, lhs.y + rhs.y, lhs.z + rhs.z);
}

bool operator<(const Particle::Triplet& lhs, const Particle::Triplet& rhs){
	if(lhs.x != rhs.x) return lhs.x < rhs.x;
	if(lhs.y != rhs.y) return lhs.y < rhs.y;
	return lhs.z < rhs.z;
}

void operator+=(Particle::Triplet& lhs, const Particle::Triplet& rhs){
	lhs = lhs + rhs;
}

void Particle::simulate(){
	v += a;
	p += v;
}

SInt64 Particle::dist() const{
	return abs(this->p.x) + abs(this->p.y) + abs(this->p.z); 
}

void first(){
	vector<Particle> vec;
	vec.resize(1000);
	SInt64 a,b,c;
	for(int i = 0; i<1000; ++i){
		cin >> a >> b >> c;
		vec[i].p = Particle::Pos(a,b,c);
		cin >> a >> b >> c;
		vec[i].v = Particle::Vel(a,b,c);
		cin >> a >> b >> c;
		vec[i].a = Particle::Acc(a,b,c);
	}
	for(int i = 0; i<1000; ++i){
		set<Particle::Pos> occupied;
		for(int j = 0; j<1000; ++j){
			if(!vec[j].collided){
				vec[j].simulate();
				set<Particle::Pos>::iterator it;
				if((it = occupied.find(vec[j].p)) != occupied.end()){
					occupied.erase(it);
					vec[j].p.used = true;
					occupied.insert(vec[j].p);
					vec[j].collided = true;
				}
				else{
					occupied.insert(vec[j].p);
				}
			}
		}
		for(int j = 0; j<1000; ++j){
			set<Particle::Pos>::iterator it;
			if(!vec[j].collided && (it = occupied.find(vec[j].p)) != occupied.end() && it->used){
				vec[j].collided = true;
			}
		}
	}
	int minid = 0;
	int mindist = vec[0].dist();
	int db = 0;
	for(int i = 0; i<1000; ++i){
		if(vec[i].dist()<mindist){
			minid = i;
			mindist = vec[i].dist();
		}
		if(!vec[i].collided) ++db;
	}
	cout << minid << endl;
	cout << db << endl;
}

int main(){
	first();
	return 0;
}