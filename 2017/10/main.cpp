#include <vector>
#include <iostream>
#include <iomanip>

using namespace std;

const int size = 256;

void first(){
    vector<int> v(size);
    for(int i = 0; i<size; ++i){v[i] = i;}
    int current = 0;
    int skipsize = 0;
    int length;
    while(cin >> length){
        vector<int> torev;
        for(int i = current; i<(current+length); ++i){
            torev.push_back(v[i%size]);
        }
        int j = torev.size()-1;
        for(int i = current; i<(current+length); ++i){
            v[i%size] = torev[j];
            --j;
        }
        current = (current + length + skipsize)%size;
        ++skipsize;
    }
    cout << v[0]*v[1] << endl;
}

void second(){
    vector<unsigned char> v(size);
    for(int i = 0; i<size; ++i){v[i] = i;}
    string s;
    cin >> s;
    vector<unsigned char> steps(s.begin(),s.end());
    steps.push_back(17);steps.push_back(31);steps.push_back(73);steps.push_back(47);steps.push_back(23);
    int current = 0;
    int skipsize = 0;
    int length;
    for(int r = 0; r<64; ++r){
        for(int k = 0; k<steps.size(); ++k){
            length = steps[k];
            vector<unsigned char> torev;
            for(int i = current; i<(current+length); ++i){
                torev.push_back(v[i%size]);
            }
            int j = torev.size()-1;
            for(int i = current; i<(current+length); ++i){
                v[i%size] = torev[j];
                --j;
            }
            current = (current + length + skipsize)%size;
            ++skipsize;
        }
    }
    
    for(int i = 0; i<16; ++i){
        unsigned char c = v[16*i];
        for(int j = 1; j<16; ++j){
            c ^= v[16*i + j];
        }
        //cout << (int)c << endl;
        cout << setfill('0') << setw(2) << hex << (int)c;
    }
    cout << endl;

}

int main(){
    second();
    return 0;
}