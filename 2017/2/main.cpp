#include <iostream>
#include <vector>

using namespace std;

vector<vector<int> > v;

int main(){
    int sol = 0;
    v.resize(16,vector<int>(16));
    for(int i = 0; i<16;  ++i){
        for(int j = 0; j<16; ++j){
            cin >> v[i][j];
            for(int k = 0; k<j; ++k){
                if(v[i][j] % v[i][k] == 0){
                    sol += v[i][j]/v[i][k];
                }
                else if(v[i][k] % v[i][j] == 0){
                    sol += v[i][k]/v[i][j];
                }
            }
        }
    }
    cout << sol << endl;
    return 0;
}