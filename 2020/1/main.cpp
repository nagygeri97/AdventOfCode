#include <iostream>
#include <vector>

using namespace std;

int main(){
    vector<int> v;
    int tmp;
    while(cin >> tmp){
        v.push_back(tmp);
    }

    for(int i = 0; i<v.size(); ++i){
        for(int j = i+1; j < v.size(); ++j){
            if(v[i] + v[j] == 2020){
                cout << v[i] * v[j] << endl;
                i = v.size();
                break;
            }
        } 
    }
    for(int i = 0; i<v.size(); ++i){
        for(int j = i+1; j < v.size(); ++j){
            for(int k = j+1; k < v.size(); ++k){
                if(v[i] + v[j] + v[k] == 2020){
                    cout << v[i] * v[j] * v[k] << endl;
                    i = v.size();
                    j = v.size();
                    break;
                }
            }
        } 
    }
}