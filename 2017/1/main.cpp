#include <string>
#include <iostream>

using namespace std;

int main(){
    string s;
    int sol = 0;
    getline(cin,s);
    /*for(int i = 0; i<s.size(); ++i){
        if(s[i] == s[(i+1)%s.size()]){
            sol += s[i] - '0';
        }
    }*/
    for(int i = 0; i<s.size(); ++i){
        if(s[i] == s[(i+s.size()/2)%s.size()]){
            sol += s[i] - '0';
        }
    }
    cout << sol << endl;
    return 0;
}