#include <iostream>
#include <vector>
#include <sstream>
#include <queue>

using namespace std;

typedef signed long long int SInt64;

struct Instruction{
	string cmd;
	bool isRegister1, isRegister2;
	SInt64 fst, snd;
};

vector<Instruction> inst;
vector<SInt64> reg;
vector<SInt64> reg2;


void first(){
	string s;
	reg.resize(26,0);
	reg2.resize(26,0);
	reg2['p'-'a'] = 1;
	while(getline(cin,s)){
		stringstream ss(s);
		inst.push_back(Instruction());
		ss >> inst[inst.size()-1].cmd;
		if(ss >> s){
			if(s.size() == 1 && s[0]-'a' >= 0 && s[0]-'z' <= 0){
				inst[inst.size()-1].isRegister1 = true;
				inst[inst.size()-1].fst = s[0]-'a';
			}
			else{
				stringstream sss(s);
				sss >> inst[inst.size()-1].fst;
			}
		}
		if(ss >> s){
			if(s.size() == 1 && s[0]-'a' >= 0 && s[0]-'z' <= 0){
				inst[inst.size()-1].isRegister2 = true;
				inst[inst.size()-1].snd = s[0]-'a';
			}
			else{
				stringstream sss(s);
				sss >> inst[inst.size()-1].snd;
			}
		}
	}
	int curr1 = 0;
	int curr2 = 0;

	int db = 0;
	queue<SInt64> snd1;
	queue<SInt64> snd2;

	SInt64 lastSnd = 0;
	while(curr1 < inst.size() && curr2 < inst.size()){
		if(inst[curr1].cmd == "set"){
			if(inst[curr1].isRegister1){
				reg[inst[curr1].fst] = inst[curr1].isRegister2? reg[inst[curr1].snd] : inst[curr1].snd;
			}
			++curr1;
		}
		else if(inst[curr1].cmd == "mul"){
			if(inst[curr1].isRegister1){
				reg[inst[curr1].fst] *= inst[curr1].isRegister2? reg[inst[curr1].snd] : inst[curr1].snd;
			}
			++curr1;
		}
		else if(inst[curr1].cmd == "add"){
			if(inst[curr1].isRegister1){
				reg[inst[curr1].fst] += inst[curr1].isRegister2? reg[inst[curr1].snd] : inst[curr1].snd;
			}
			++curr1;
		}
		else if(inst[curr1].cmd == "mod"){
			if(inst[curr1].isRegister1){
				reg[inst[curr1].fst] %= inst[curr1].isRegister2? reg[inst[curr1].snd] : inst[curr1].snd;
			}
			++curr1;
		}
		else if(inst[curr1].cmd == "snd"){
			if(inst[curr1].isRegister1){
				snd1.push(reg[inst[curr1].fst]);
			}
			else{
				snd1.push(inst[curr1].fst);
			}
			++curr1;
		}
		else if(inst[curr1].cmd == "rcv" && !snd2.empty()){
			if(inst[curr1].isRegister1){
				reg[inst[curr1].fst] = snd2.front();
				snd2.pop();
			}
			else{
				snd2.pop();
			}
			++curr1;
		}
		else if(inst[curr1].cmd == "jgz"){
			if(inst[curr1].isRegister1){
				if(reg[inst[curr1].fst] > 0){
					curr1 += inst[curr1].isRegister2? reg[inst[curr1].snd] : inst[curr1].snd;
				}
				else{
					++curr1;
				}
			}
			else{
				if(inst[curr1].fst > 0){
					curr1 += inst[curr1].isRegister2? reg[inst[curr1].snd] : inst[curr1].snd;
				}
				else{
					++curr1;
				}
			}
		}


		if(inst[curr2].cmd == "set"){
			if(inst[curr2].isRegister1){
				reg2[inst[curr2].fst] = inst[curr2].isRegister2? reg2[inst[curr2].snd] : inst[curr2].snd;
			}
			++curr2;
		}
		else if(inst[curr2].cmd == "mul"){
			if(inst[curr2].isRegister1){
				reg2[inst[curr2].fst] *= inst[curr2].isRegister2? reg2[inst[curr2].snd] : inst[curr2].snd;
			}
			++curr2;
		}
		else if(inst[curr2].cmd == "add"){
			if(inst[curr2].isRegister1){
				reg2[inst[curr2].fst] += inst[curr2].isRegister2? reg2[inst[curr2].snd] : inst[curr2].snd;
			}
			++curr2;
		}
		else if(inst[curr2].cmd == "mod"){
			if(inst[curr2].isRegister1){
				reg2[inst[curr2].fst] %= inst[curr2].isRegister2? reg2[inst[curr2].snd] : inst[curr2].snd;
			}
			++curr2;
		}
		else if(inst[curr2].cmd == "snd"){
			if(inst[curr2].isRegister1){
				snd2.push(reg2[inst[curr2].fst]);
				++db;
			}
			else{
				snd2.push(inst[curr2].fst);
				++db;
			}
			++curr2;
		}
		else if(inst[curr2].cmd == "rcv" && !snd1.empty()){
			if(inst[curr2].isRegister1){
				reg2[inst[curr2].fst] = snd1.front();
				snd1.pop();
			}
			else{
				snd1.pop();
			}
			++curr2;
		}
		else if(inst[curr2].cmd == "jgz"){
			if(inst[curr2].isRegister1){
				if(reg2[inst[curr2].fst] > 0){
					curr2 += inst[curr2].isRegister2? reg2[inst[curr2].snd] : inst[curr2].snd;
				}
				else{
					++curr2;
				}
			}
			else{
				if(inst[curr2].fst > 0){
					curr2 += inst[curr2].isRegister2? reg2[inst[curr2].snd] : inst[curr2].snd;
				}
				else{
					++curr2;
				}
			}
		}

		cout << db << endl;

	}

}

int main(){
	first();
	return 0;
}