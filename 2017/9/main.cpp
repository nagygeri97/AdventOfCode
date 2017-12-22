#include <iostream>
#include <string>

using namespace std;

void first(){
    string s;
    cin >> s;
    int score = 0;
    int level = 1;
    bool isNextIgnored = false;
    bool inGarbage = false;
    int count = 0;
    for(int i = 0; i<s.size(); ++i){
        if(!inGarbage && !isNextIgnored){
            if(s[i] == '{'){
                score += level;
                level++;
            }
            else if(s[i] == '}'){
                level--;
            }
            else if(s[i] == '<'){
                inGarbage = true;
            }
        }
        else if(inGarbage && !isNextIgnored){
            if(s[i] == '>'){
                inGarbage = false;
            }
            else if(s[i] != '!'){
                count++;
            }
        }
        if(!isNextIgnored){
            if(s[i] == '!'){
                isNextIgnored = true;
            }
        }
        else{
            isNextIgnored = false;
        }
    }
    cout << count << endl;
}

int main(){
    first();
    return 0;
}