#include <iostream>

using namespace std;

int main(){
    int h = 0;
    for(int b = 108400; b<=125400; b+=17){
        bool f = true;
        for(int d = 2; d<b; ++d){
            if(b % d == 0) f = false;
        }
        if(!f) ++h;
    }
    cout << h << endl;
    return 0;
}