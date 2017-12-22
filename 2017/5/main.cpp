#include <iostream>
#include <vector>

using namespace std;

void first(){ //and second;
    vector<int> v;
    int n;
    while(cin >> n){
        v.push_back(n);
    }

    int curr = 0;
    int db = 0;
    while(curr >= 0 && curr < v.size()){
        int pc = curr;
        curr += v[curr];
        if(v[pc] >= 3){
            --v[pc];
        }
        else{
            ++v[pc];
        }
        ++db;
    }
    cout << db << endl;
}

int main(){
    first();
    return 0;
}