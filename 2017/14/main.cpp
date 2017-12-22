#include <vector>
#include <iostream>
#include <iomanip>
#include <utility>
#include <queue>

using namespace std;

const int size = 256;

vector<vector<bool>> table;

void first(){
    string str;
    cin >> str;
    table.resize(128,vector<bool>(128,false));
    int used = 0;
    for(int q = 0; q < 128; ++q){
        vector<unsigned char> v(size);
        for(int i = 0; i<size; ++i){v[i] = i;}
        string s = str + "-" + to_string(q);
        //cout << s << endl;
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
        
        int k = 0;
        for(int i = 0; i<16; ++i){
            unsigned char c = v[16*i];
            for(int j = 1; j<16; ++j){
                c ^= v[16*i + j];
            }

            for (int j = 0; j < 8; ++j)
            {
                // if(c&1)cout << "#";
                // else cout << ".";
                if(c & 1) table[q][k+7-j] = true;
                used += c & 1;
                c >>= 1;
            }
            k+=8;
            //cout << (int)c << endl;
            //cout << setfill('0') << setw(2) << hex << (int)c;
        }
        //cout << endl;
    }
    //cout << used << endl;

    // for(int i = 0; i<128; ++i){
    //     for(int j = 0; j<128; ++j){
    //         cout << table[i][j];
    //     }
    //     cout << endl;
    // }
}

void second(){
    int comp = 0;
    for(int i = 0; i<128; ++i){
        for(int j = 0; j<128; ++j){
            if(table[i][j]){
                ++comp;
                queue<pair<int,int>> q;
                q.push(make_pair(i,j));
                while(!q.empty()){
                    pair<int,int> p  = q.front();
                    q.pop();
                    if(table[p.first][p.second]){
                        table[p.first][p.second] = false;
                        if(p.first - 1 >= 0 && table[p.first-1][p.second]){
                            q.push(make_pair(p.first-1,p.second));
                        }
                        if(p.first + 1 < 128 && table[p.first+1][p.second]){
                            q.push(make_pair(p.first+1,p.second));
                        }
                        if(p.second - 1 >= 0 && table[p.first][p.second-1]){
                            q.push(make_pair(p.first,p.second-1));
                        }
                        if(p.second + 1 < 128 && table[p.first][p.second+1]){
                            q.push(make_pair(p.first,p.second+1));
                        }
                    }
                }
            }
        }
    }
    cout << comp << endl;
}

int main(){
    first();
    second();
    return 0;
}