#include <iostream>
#include <vector>
#include <queue>
#include <map>
#include <algorithm>
#include <cstdint>

using namespace std;

enum Mode{POSITION = 0, IMMEDIATE = 1, RELATIVE = 2};

enum Opcode{ADD = 1, MUL = 2, READ = 3, WRITE = 4, JTRUE = 5, JFALSE = 6, LT = 7, EQ = 8, RELBASE = 9, HALT = 99};

class Program{
public:
	Program(vector<int64_t> v) : isHalted(false), isPaused(true), v(v), i(0), relBase(0), inputs() {};
	bool isHalted;
	bool isPaused;

	vector<int64_t> run(){
		isPaused = false;
		vector<int64_t> outputs;
		while(!(isHalted || isPaused)){
			int instr = v[i];
			vector<Mode> modes;
			Opcode opcode = static_cast<Opcode>(instr % 100);
			instr /= 100;
			for(int i = 0; i<3; ++i){
				modes.emplace_back(static_cast<Mode>(instr % 10));
				instr /= 10;
			}
			switch(opcode){
				case ADD:
					pv(modes, 3) = pv(modes, 1) + pv(modes, 2);
					i += 4;
					break;
				case MUL:
					pv(modes, 3) = pv(modes, 1) * pv(modes, 2);
					i += 4;
					break;
				case READ:
					if(inputs.empty()){
						isPaused = true;
					}
					else{
						pv(modes, 1) = inputs.front();
						inputs.pop();
						i += 2;
					}
					break;
				case WRITE:
					outputs.push_back(pv(modes, 1));
					i += 2;
					break;
				case JTRUE:
					if(pv(modes, 1) != 0){
						i = pv(modes, 2);
					}
					else{
						i += 3;
					}
					break;
				case JFALSE:
					if(pv(modes, 1) == 0){
						i = pv(modes, 2);
					}
					else{
						i += 3;
					}
					break;
				case LT:
					if(pv(modes, 1) < pv(modes, 2)){
						pv(modes, 3) = 1;
					}
					else{
						pv(modes, 3) = 0;
					}
					i += 4;
					break;
				case EQ:
					if(pv(modes, 1) == pv(modes, 2)){
						pv(modes, 3) = 1;
					}
					else{
						pv(modes, 3) = 0;
					}
					i += 4;
					break;
				case RELBASE:
					relBase += pv(modes, 1);
					i += 2;
					break;
				case HALT:
					isHalted = true;
					break;				
				default:
					cout << "ERROR: " << opcode << endl;
			}
			
		}
		return outputs;
	}
	void setInput(queue<int64_t>& ins){
		while(!ins.empty()){
			inputs.push(ins.front());
			ins.pop();
		}
	}
private:
	vector<int64_t> v;
	map<int, int64_t> extraMemory;
	int i;
	int relBase;
	queue<int64_t> inputs;

	int64_t& pv(vector<Mode>& modes, int parameter){
		switch(modes[parameter - 1]){
			case POSITION: if(v[i + parameter] < v.size()) return v[v[i + parameter]];
						   else return extraMemory[v[i + parameter]];
			case IMMEDIATE: return v[i + parameter];
			case RELATIVE: if(relBase + v[i + parameter] < v.size()) return v[relBase + v[i + parameter]];
						   else return extraMemory[relBase + v[i + parameter]];
		}
	}
};

int& x(int& a){
	return a;
}

int main(){
	vector<int64_t> v;
	int s;
	while(cin >> s){
		v.push_back(s);
	}
	// Part 1
	queue<int64_t> inputs;
	inputs.push(1);
	Program p(v);
	p.setInput(inputs);
	auto res = p.run();
	for(auto r : res){
		cout << r << endl;
	}
	//Part 2
	inputs = queue<int64_t>{};
	inputs.push(2);
	p = Program(v);
	p.setInput(inputs);
	res = p.run();
	for(auto r : res){
		cout << r << endl;
	}
}