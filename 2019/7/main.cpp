#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

enum Mode{POSITION = 0, IMMEDIATE = 1};

enum Opcode{ADD = 1, MUL = 2, READ = 3, WRITE = 4, JTRUE = 5, JFALSE = 6, LT = 7, EQ = 8, HALT = 99};

class Program{
public:
	Program(vector<int> v) : isHalted(false), isPaused(true), v(v), i(0), inputs() {};
	bool isHalted;
	bool isPaused;

	vector<int> run(){
		isPaused = false;
		vector<int> outputs;
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
					v[v[i + 3]] = getParameterValue(v, modes[0], v[i + 1]) + getParameterValue(v, modes[1], v[i + 2]);
					i += 4;
					break;
				case MUL:
					v[v[i + 3]] = getParameterValue(v, modes[0], v[i + 1]) * getParameterValue(v, modes[1], v[i + 2]);
					i += 4;
					break;
				case READ:
					if(inputs.empty()){
						isPaused = true;
					}
					else{
						v[v[i + 1]] = inputs.front();
						inputs.pop();
						i += 2;
					}
					break;
				case WRITE:
					outputs.push_back(getParameterValue(v, modes[0], v[i + 1]));
					i += 2;
					break;
				case JTRUE:
					if(getParameterValue(v, modes[0], v[i + 1]) != 0){
						i = getParameterValue(v, modes[1], v[i + 2]);
					}
					else{
						i += 3;
					}
					break;
				case JFALSE:
					if(getParameterValue(v, modes[0], v[i + 1]) == 0){
						i = getParameterValue(v, modes[1], v[i + 2]);
					}
					else{
						i += 3;
					}
					break;
				case LT:
					if(getParameterValue(v, modes[0], v[i + 1]) < getParameterValue(v, modes[1], v[i + 2])){
						v[v[i + 3]] = 1;
					}
					else{
						v[v[i + 3]] = 0;
					}
					i += 4;
					break;
				case EQ:
					if(getParameterValue(v, modes[0], v[i + 1]) == getParameterValue(v, modes[1], v[i + 2])){
						v[v[i + 3]] = 1;
					}
					else{
						v[v[i + 3]] = 0;
					}
					i += 4;
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
	void setInput(queue<int>& ins){
		while(!ins.empty()){
			inputs.push(ins.front());
			ins.pop();
		}
	}
private:
	vector<int> v;
	int i;
	queue<int> inputs;

	int getParameterValue(const vector<int>& v, Mode mode, int index){
		switch(mode){
			case POSITION: return v[index];
			case IMMEDIATE: return index;
		}
	}
};

int main(){
	vector<int> v;
	int s;
	while(cin >> s){
		v.push_back(s);
	}
	// Part 1
	vector<int> p{0,1,2,3,4};
	int maxResult = 0;
	do{
		int result = 0;
		vector<Program> progs(5, Program(v));
		for(int i = 0; i<=4; ++i){
			queue<int> inputs;
			inputs.push(p[i]);
			inputs.push(result);
			progs[i].setInput(inputs);
			auto res = progs[i].run();
			result = res[0];
		}
		if(result > maxResult){
			maxResult = result;
		}
	}while(next_permutation(p.begin(), p.end()));
	cout << maxResult << endl;

	// Part 2
	p = {5,6,7,8,9};
	maxResult = 0;
	do{
		vector<int> result(1,0);
		int finalRes = 0;
		vector<Program> progs(5, Program(v));
		int haltCount = 0;
		int i = -1;
		while(haltCount < 5){
			++i;
			if(progs[i % 5].isHalted) continue;
			queue<int> inputs;
			if(i < 5){
				inputs.push(p[i]);
			}
			for(const auto& res : result){
				inputs.push(res);
			}
			progs[i % 5].setInput(inputs);
			result = progs[i % 5].run();
			if(progs[i % 5].isHalted) ++haltCount;
			if(i % 5 == 4 && result.size() > 0){
				finalRes = result[result.size() - 1];
			}
		}
		if(finalRes > maxResult){
			maxResult = finalRes;
		}
	}while(next_permutation(p.begin(), p.end()));
	cout << maxResult << endl;
}