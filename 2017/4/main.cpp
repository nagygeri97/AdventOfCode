#include <iostream>
#include <sstream>
#include <set>
#include <algorithm>

using namespace std;

void first(){
    string str;
    int valid = 0;
    while(getline(cin,str)){
        stringstream s(str);
        set<string> se;
        string st;
        bool v = true;
        while(s >> st){
            if(se.count(st) == 0){
                se.insert(st);
            }
            else{
                v = false;
                break;
            }
        }
        if(v) ++valid;
    }
    cout << valid << endl;
}
void second(){
    string str;
    int valid = 0;
    while(getline(cin,str)){
        stringstream s(str);
        set<string> se;
        string st;
        bool v = true;
        while(s >> st){
            sort(st.begin(),st.end());
            if(se.count(st) == 0){
                se.insert(st);
            }
            else{
                v = false;
                break;
            }
        }
        if(v) ++valid;
    }
    cout << valid << endl;
}

int main(){
    second();
    return 0;
}