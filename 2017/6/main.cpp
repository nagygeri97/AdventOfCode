#include <iostream>
#include <vector>
#include <set>

using namespace std;

void first(){
    vector<int> blocks;
    set<vector<int>> s;
    int tmp;
    int db = 0;
    while(cin >> tmp){
        blocks.push_back(tmp);
    }
    while(s.count(blocks) == 0){
        ++db;
        s.insert(blocks);
        int maxi = 0;
        for(int i = 1; i<blocks.size();++i){
            if(blocks[i] > blocks[maxi]){
                maxi = i;
            }
        }
        int mv = blocks[maxi];
        blocks[maxi] = 0;
        int currInd = (maxi + 1)%blocks.size();
        for(int i = mv; i > 0; --i){
            ++blocks[currInd];
            currInd = (currInd + 1)%blocks.size();
        }
    }
    cout << db << endl;
}

void second(){
    vector<int> blocks;
    set<vector<int>> s;
    int tmp;
    int db = 0;
    while(cin >> tmp){
        blocks.push_back(tmp);
    }
    while(s.count(blocks) == 0){
        s.insert(blocks);
        int maxi = 0;
        for(int i = 1; i<blocks.size();++i){
            if(blocks[i] > blocks[maxi]){
                maxi = i;
            }
        }
        int mv = blocks[maxi];
        blocks[maxi] = 0;
        int currInd = (maxi + 1)%blocks.size();
        for(int i = mv; i > 0; --i){
            ++blocks[currInd];
            currInd = (currInd + 1)%blocks.size();
        }
    }
    vector<int> pb = blocks;
    do{
        ++db;
        s.insert(blocks);
        int maxi = 0;
        for(int i = 1; i<blocks.size();++i){
            if(blocks[i] > blocks[maxi]){
                maxi = i;
            }
        }
        int mv = blocks[maxi];
        blocks[maxi] = 0;
        int currInd = (maxi + 1)%blocks.size();
        for(int i = mv; i > 0; --i){
            ++blocks[currInd];
            currInd = (currInd + 1)%blocks.size();
        }
    }while(pb != blocks);
    cout << db << endl;
}

int main(){
    //first();
    second();
    return 0;
}