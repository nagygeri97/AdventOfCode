#include <iostream>
#include <set>
#include <map>
#include <vector>
#include <algorithm>

using namespace std;

int r[4];

void resetRegisters(){
	for(int i = 0; i<4; ++i) r[i] = 0;
}

void fuckUpRegisters(){
	for(int i = 0; i<4; ++i) r[i] = 99999999;
}

void addr(int a, int b, int c){
	if(a < 0 || a > 3 || b < 0 || b > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	r[c] = r[a] + r[b];
}
void addi(int a, int b, int c){
	if(a < 0 || a > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	r[c] = r[a] + b;
}
void mulr(int a, int b, int c){
	if(a < 0 || a > 3 || b < 0 || b > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	r[c] = r[a] * r[b];
}
void muli(int a, int b, int c){
	if(a < 0 || a > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	r[c] = r[a] * b;
}
void banr(int a, int b, int c){
	if(a < 0 || a > 3 || b < 0 || b > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	r[c] = r[a] & r[b];
}
void bani(int a, int b, int c){
	if(a < 0 || a > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	r[c] = r[a] & b;
}
void borr(int a, int b, int c){
	if(a < 0 || a > 3 || b < 0 || b > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	r[c] = r[a] | r[b];
}
void bori(int a, int b, int c){
	if(a < 0 || a > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	r[c] = r[a] | b;
}
void setr(int a, int b, int c){
	if(a < 0 || a > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	r[c] = r[a];
}
void seti(int a, int b, int c){
	if(c < 0 || c > 3) {fuckUpRegisters(); return;}
	r[c] = a;
}
void gtir(int a, int b, int c){
	if(b < 0 || b > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	if(a > r[b]) r[c] = 1;
	else r[c] = 0;
}
void gtri(int a, int b, int c){
	if(a < 0 || a > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	if(r[a] > b) r[c] = 1;
	else r[c] = 0;
}
void gtrr(int a, int b, int c){
	if(a < 0 || a > 3 || b < 0 || b > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	if(r[a] > r[b]) r[c] = 1;
	else r[c] = 0;
}
void eqir(int a, int b, int c){
	if(b < 0 || b > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	if(a == r[b]) r[c] = 1;
	else r[c] = 0;
}
void eqri(int a, int b, int c){
	if(a < 0 || a > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	if(r[a] == b) r[c] = 1;
	else r[c] = 0;
}
void eqrr(int a, int b, int c){
	if(a < 0 || a > 3 || b < 0 || b > 3 || c < 0 || c > 3) {fuckUpRegisters(); return;}
	if(r[a] == r[b]) r[c] = 1;
	else r[c] = 0;
}

void setRegisters(vector<int>& values){
	for(int i = 0; i<4; ++i) r[i] = values[i];
}

bool checkRegisters(vector<int>& values){
	for(int i = 0; i<4; ++i){
		if(r[i] != values[i]) return false;
	}
	return true;
}

typedef void (*Command)(int,int,int);
vector<Command> commands{&addr, &addi, &mulr, &muli, &banr, &bani, &borr, &bori, &setr, &seti, &gtir, &gtri, &gtrr, &eqir, &eqri, &eqrr};

vector<set<Command>> commandSets(16,{&addr, &addi, &mulr, &muli, &banr, &bani, &borr, &bori, &setr, &seti, &gtir, &gtri, &gtrr, &eqir, &eqri, &eqrr});
vector<Command> finalCommands(16,nullptr);
map<Command,string> commandNames;

bool test(vector<int>& input, vector<int>& command, vector<int>& output){
	int count = 0;
	set<Command> possibilities;
	for(Command cmd : commands){
		setRegisters(input);
		cmd(command[1],command[2],command[3]);
		if(checkRegisters(output)){
			++count;
			possibilities.insert(cmd);
		}
	}
	set<Command> intersect;
	set_intersection(commandSets[command[0]].begin(), commandSets[command[0]].end(), possibilities.begin(), possibilities.end(), std::inserter(intersect,intersect.begin()));
	commandSets[command[0]] = intersect;
	if(count >= 3) return true;
	else return false;
}


void read(){
	vector<int> input(4);
	vector<int> command(4);
	vector<int> output(4);
	int result = 0;
	int a = 0;
	while(a<815){
		for(int i = 0; i<4; ++i) cin >> input[i];
		for(int i = 0; i<4; ++i) cin >> command[i];
		for(int i = 0; i<4; ++i) cin >> output[i];

		if(test(input,command,output)){
			++result;
		} 
		++a;
	}
	cout << "Part 1: " << result << endl;
	for(int i = 0; i<16; ++i){
		for(int j = 0; j<16; ++j){
			if(finalCommands[j] != nullptr) continue;
			if(commandSets[j].size() == 1){
				Command cmd = *commandSets[j].begin();
				finalCommands[j] = cmd;
				for(int k = 0; k<16; ++k){
					if(j == k) continue;
					commandSets[k].erase(cmd);
				}
			}
			
		}
	}
}

void read2(){
	int command, a, b, c;
	resetRegisters();
	while(cin >> command >> a >> b >> c){
		finalCommands[command](a,b,c);
	}
	cout << "Part 2: " << r[0] << endl;
}

int main(){
	commandNames[&addr] = "addr";
	commandNames[&addi] = "addi";
	commandNames[&mulr] = "mulr";
	commandNames[&muli] = "muli";
	commandNames[&banr] = "banr";
	commandNames[&bani] = "bani";
	commandNames[&borr] = "borr";
	commandNames[&bori] = "bori";
	commandNames[&setr] = "setr";
	commandNames[&seti] = "seti";
	commandNames[&gtir] = "gtir";
	commandNames[&gtri] = "gtri";
	commandNames[&gtrr] = "gtrr";
	commandNames[&eqir] = "eqir";
	commandNames[&eqri] = "eqri";
	commandNames[&eqrr] = "eqrr";
	read();
	for(int i = 0; i<16; ++i){
		cout << i << " " << commandNames[finalCommands[i]] << endl;
	}
	read2();
}