#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

enum Mode{POSITION = 0, IMMEDIATE = 1};

enum Opcode{ADD = 1, MUL = 2, READ = 3, WRITE = 4, JTRUE = 5, JFALSE = 6, LT = 7, EQ = 8, HALT = 99};

int getParameterValue(const vector<int>& v, Mode mode, int index){
	switch(mode){
		case POSITION: return v[index];
		case IMMEDIATE: return index;
	}
}

vector<int> run(vector<int> v, queue<int>& inputs){
	int i = 0;
	vector<int> outputs;
	bool done = false;
	while(!done){
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
				v[v[i + 1]] = inputs.front();
				inputs.pop();
				i += 2;
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
				done = true;
				break;
			default:
				cout << "ERROR: " << opcode << endl;
		}
		
	}
	return outputs;
}

int main(){
	vector<int> v;
	int s;
	while(cin >> s){
		v.push_back(s);
	}
	// Part 1
	queue<int> inputs;
	inputs.push(1);
	auto outputs = run(v, inputs);
	cout << outputs[outputs.size() - 1] << endl;
	// Part 2
	inputs.push(5);
	outputs = run(v, inputs);
	cout << outputs[outputs.size() - 1] << endl;
}