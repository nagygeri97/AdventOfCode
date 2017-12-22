#include <iostream>
#include <map>

using namespace std;

void first(){
    int max = 0;
    map<string,int> m;
    string reg; string instr; int val; string regif; string op; int valif; string tmp;
    while(cin >> reg >> instr >> val >> tmp >> regif >> op >> valif){
        bool ifexpr = false;
        if(op == "==") ifexpr = m[regif] == valif;
        else if(op == "!=") ifexpr = m[regif] != valif;
        else if(op == "<=") ifexpr = m[regif] <= valif;
        else if(op == ">=") ifexpr = m[regif] >= valif;
        else if(op == "<") ifexpr = m[regif] < valif;
        else if(op == ">") ifexpr = m[regif] > valif;

        if(ifexpr){
            if(instr == "dec") m[reg] -= val;
            else if(instr == "inc") m[reg] += val;
            if(m[reg] > max) max = m[reg];
        }
    }

    // int max = m.begin()->second;
    // for (std::map<string,int>::iterator it=m.begin(); it!=m.end(); ++it){
    //     if(it->second > max) max = it->second;
    // }
    cout << max << endl;
}

int main(){
    first();
    return 0;
}