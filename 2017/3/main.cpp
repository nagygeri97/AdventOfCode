#include <iostream>
#include <cmath>
#include <vector>

using namespace std;

int main(){
    /*
    int x;
    cin >> x;
    int sq = sqrt(x);
    if(sq % 2 == 0) --sq;
    if(x == sq*sq) {cout << sq-1 << endl; return 0;}
    int d = sq-1;
    int dif = x-sq*sq;
    dif = dif%(sq+1);
    if(dif == 0) dif += sq + 1;
    cout << abs(dif - (sq+1)/2) + (sq+1)/2 << endl;
    */
    int l;
    cin >> l;
    vector<vector<int>> v;
    v.resize(20,vector<int>(20,0));
    int x = 10, y = 10;
    int dir = 1;
    int step = 1;
    int stepincr = 0;
    int s;
    int end;
    v[x][y] = 1;
    while(true){

        switch(dir){
            case 1: {
                end = x+step;
                ++x;
                for(;x<=end;++x){
                    s = 0;
                    for(int i =  x-1; i <= x+1; ++i){
                        for(int j = y-1; j <= y+1; ++j){
                            s += v[i][j];
                        }
                    }
                    v[x][y] = s;
                    if(s > l){
                        cout << s << endl;
                        return 0;
                    }
                }
                --x;
                break;
            }
            case 2: {
                end = y+step;
                ++y;
                for(;y<=end;++y){
                    s = 0;
                    for(int i =  x-1; i <= x+1; ++i){
                        for(int j = y-1; j <= y+1; ++j){
                            s += v[i][j];
                        }
                    }
                    v[x][y] = s;
                    if(s > l){
                        cout << s << endl;
                        return 0;
                    }
                }
                --y;
                break;
            }
            case 3: {
                end = x-step;
                --x;
                for(;x>=end;--x){
                    s = 0;
                    for(int i =  x-1; i <= x+1; ++i){
                        for(int j = y-1; j <= y+1; ++j){
                            s += v[i][j];
                        }
                    }
                    v[x][y] = s;
                    if(s > l){
                        cout << s << endl;
                        return 0;
                    }
                }
                ++x;
                break;
            }
            case 0: {
                end = y-step;
                --y;
                for(;y>=end;--y){
                    s = 0;
                    for(int i =  x-1; i <= x+1; ++i){
                        for(int j = y-1; j <= y+1; ++j){
                            s += v[i][j];
                        }
                    }
                    v[x][y] = s;
                    if(s > l){
                        cout << s << endl;
                        return 0;
                    }
                }
                ++y;
                break;
            }
        }
        ++dir;
        dir %= 4;
        if(stepincr == 1){
            stepincr = 0;
            ++step;
        }
        else{
            stepincr = 1;
        }
    }
    return 0;
}