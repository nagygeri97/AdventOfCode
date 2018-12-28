#include <iostream>
#include <vector>
#include <map>

using namespace std;

int r[6];
int ip;

void resetRegisters(){
	for(int i = 0; i<6; ++i) r[i] = 0;
}

void addr(int a, int b, int c){
	r[c] = r[a] + r[b];
}
void addi(int a, int b, int c){
	r[c] = r[a] + b;
}
void mulr(int a, int b, int c){
	r[c] = r[a] * r[b];
}
void muli(int a, int b, int c){
	r[c] = r[a] * b;
}
void banr(int a, int b, int c){
	r[c] = r[a] & r[b];
}
void bani(int a, int b, int c){
	r[c] = r[a] & b;
}
void borr(int a, int b, int c){
	r[c] = r[a] | r[b];
}
void bori(int a, int b, int c){
	r[c] = r[a] | b;
}
void setr(int a, int b, int c){
	r[c] = r[a];
}
void seti(int a, int b, int c){
	r[c] = a;
}
void gtir(int a, int b, int c){
	if(a > r[b]) r[c] = 1;
	else r[c] = 0;
}
void gtri(int a, int b, int c){
	if(r[a] > b) r[c] = 1;
	else r[c] = 0;
}
void gtrr(int a, int b, int c){
	if(r[a] > r[b]) r[c] = 1;
	else r[c] = 0;
}
void eqir(int a, int b, int c){
	if(a == r[b]) r[c] = 1;
	else r[c] = 0;
}
void eqri(int a, int b, int c){
	if(r[a] == b) r[c] = 1;
	else r[c] = 0;
}
void eqrr(int a, int b, int c){
	if(r[a] == r[b]) r[c] = 1;
	else r[c] = 0;
}

struct Instruction{
	string cmd;
	int a, b, c;
	Instruction(){}
	Instruction(string cmd, int a, int b, int c): cmd(cmd), a(a), b(b), c(c){} 
};

typedef void (*Command)(int,int,int);
map<string,Command> commands;
vector<Instruction> instructions;

void read(){
	string cmd;
	int a, b, c;
	cin >> cmd >> ip;
	while(cin >> cmd >> a >> b >> c){
		instructions.emplace_back(cmd, a, b, c);
	}
	r[ip] = -1;
}

void registerDump(int x, Instruction& ins){
	for(int i = 0; i<6; cout << r[i] << "\t", ++i);
	cout << endl;
}

void run(){
	while(true){
		++r[ip];
		if(r[ip] >= instructions.size() || r[ip] < 0) return;
		Instruction& curr = instructions[r[ip]];
		commands[curr.cmd](curr.a, curr.b, curr.c);
	}
}

void run2(){
	int asd = 0;
	while(asd < 1000){
		++r[ip];
		if(r[ip] >= instructions.size() || r[ip] < 0) return;
		Instruction& curr = instructions[r[ip]];
		commands[curr.cmd](curr.a, curr.b, curr.c);
		//registerDump(asd, curr);
		++asd;
	}
}

int main(){
	commands["addr"] = &addr;
	commands["addi"] = &addi;
	commands["mulr"] = &mulr;
	commands["muli"] = &muli;
	commands["banr"] = &banr;
	commands["bani"] = &bani;
	commands["borr"] = &borr;
	commands["bori"] = &bori;
	commands["setr"] = &setr;
	commands["seti"] = &seti;
	commands["gtir"] = &gtir;
	commands["gtri"] = &gtri;
	commands["gtrr"] = &gtrr;
	commands["eqir"] = &eqir;
	commands["eqri"] = &eqri;
	commands["eqrr"] = &eqrr;
	read();
	run();
	cout << "Part 1: " <<  r[0] << endl;
	resetRegisters();
	r[0] = 1;
	r[ip] = -1;
	run2();
	// Observation: 
	// It calculates the sum of divisors to a number
	// After the first few instructions that number is stored in r[3]
	int s = 0;
	for(int i = 1; i<=r[3]; ++i){
		if(r[3]%i == 0) s += i;
	}
	cout << "Part 2: " << s << endl;
}