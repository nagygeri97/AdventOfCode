#include <iostream>
#include <vector>
#include <iomanip>
#include <sstream>

using namespace std;

typedef signed long long int SInt64;

struct Instruction{
    string inst;
    bool isReg1;
    bool isReg2;
    int arg1;
    int arg2;
    Instruction(){};
    Instruction(string s, int a1, int a2, bool b1, bool b2):inst(s),isReg1(b1),isReg2(b2),arg1(a1),arg2(a2){}
};

SInt64 reg[8];

void first(){
    reg[0] = 0;
    vector<Instruction> ins;
    string s, s1, s2; bool b1, b2;
    int a1, a2;
    while(cin >> s >> s1 >> s2){
        if(s1.size() > 1 || !(s1[0] - 'a' >= 0 && s1[0] - 'z' <= 0)){
            b1 = false;
            stringstream ss(s1);
            ss >> a1;
        }
        else{
            b1 = true;
            a1 = s1[0] - 'a';
        }
        if(s2.size() > 1 || !(s2[0] - 'a' >= 0 && s2[0] - 'z' <= 0)){
            b2 = false;
            stringstream ss(s2);
            ss >> a2;
        }
        else{
            b2 = true;
            a2 = s2[0] - 'a';
        }

        ins.push_back(Instruction(s,a1,a2,b1,b2));
    }

    int curr = 0;
    while(curr >= 0 && curr < ins.size()){
        cout << "curr: " << curr + 1 << endl;
        if(ins[curr].inst == "set"){
            reg[ins[curr].arg1] = ins[curr].isReg2?reg[ins[curr].arg2]:ins[curr].arg2;
            ++curr;
        }
        else if(ins[curr].inst == "sub"){
            reg[ins[curr].arg1] -= ins[curr].isReg2?reg[ins[curr].arg2]:ins[curr].arg2;
            ++curr;
        }
        else if(ins[curr].inst == "mul"){
            reg[ins[curr].arg1] *= ins[curr].isReg2?reg[ins[curr].arg2]:ins[curr].arg2;
            ++curr;
        }
        else if(ins[curr].inst == "jnz"){
            if((ins[curr].isReg1?reg[ins[curr].arg1]:ins[curr].arg1) != 0){
                curr += ins[curr].isReg2?reg[ins[curr].arg2]:ins[curr].arg2;
            }
            else ++curr;
        }
        for(int i = 0; i < 8; ++i){
            cout << setw(7) << (char)(i+'a');
        }
        cout << endl;
        for(int i = 0; i < 8; ++i){
            cout << setw(7) << reg[i];
        }
        cout << endl;
    }
    cout << reg[7] << endl;
}

int main(){
    first();
    return 0;
}