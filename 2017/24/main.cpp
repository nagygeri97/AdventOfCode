#include <iostream>
#include <vector>
#include <utility>

using namespace std;

struct Piece{
    int a,b;
    bool used;
    Piece(int a, int b):used(false),a(a),b(b){}
};

vector<Piece> pieces;
vector<vector<int>> as;
vector<vector<int>> bs;

pair<int,int> pathFind(int currPiece, bool useA, int weight, int depth){
    pieces[currPiece].used = true;
    int next = useA?pieces[currPiece].b:pieces[currPiece].a;

    pair<int,int> max = pair<int,int>(depth,weight);
    for(int i = 0; i<as[next].size(); ++i){
        if(!pieces[as[next][i]].used){
            pair<int,int> tmp = pathFind(as[next][i],true,weight + pieces[as[next][i]].a + pieces[as[next][i]].b, depth + 1);
            if(tmp.first > max.first || (tmp.first == max.first && tmp.second > max.second)) max = tmp;
        }
    }
    for(int i = 0; i<bs[next].size(); ++i){
        if(!pieces[bs[next][i]].used){
            pair<int,int> tmp = pathFind(bs[next][i],false,weight + pieces[bs[next][i]].a + pieces[bs[next][i]].b, depth + 1);
            if(tmp.first > max.first || (tmp.first == max.first && tmp.second > max.second)) max = tmp;
        }
    }

    pieces[currPiece].used = false;
    return max;
}

void first(){
    int a,b;
    int i = 0;
    as.resize(51);
    bs.resize(51);
    while(cin >> a >> b){
        pieces.push_back(Piece(a,b));
        as[a].push_back(i);
        bs[b].push_back(i);
        ++i;
    }
    pair<int,int> max = pair<int,int>(0,0);
    for(int i = 0; i<as[0].size(); ++i){
        if(!pieces[as[0][i]].used){
            pair<int,int> tmp = pathFind(as[0][i],true,pieces[as[0][i]].a + pieces[as[0][i]].b,0);
            if(tmp.first > max.first || (tmp.first == max.first && tmp.second > max.second)) max = tmp;
        }
    }
    for(int i = 0; i<bs[0].size(); ++i){
        if(!pieces[bs[0][i]].used){
            pair<int,int> tmp = pathFind(bs[0][i],false,pieces[bs[0][i]].a + pieces[bs[0][i]].b,0);
            if(tmp.first > max.first || (tmp.first == max.first && tmp.second > max.second)) max = tmp;
        }
    }
    cout << max.first << " " << max.second << endl;
}

int main(){
    first();
    return 0;
}