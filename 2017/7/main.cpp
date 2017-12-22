#include <iostream>
#include <map>
#include <sstream>
#include <vector>
#include <algorithm>

using namespace std;

void first(){
    map<string,bool> m;
    vector<string> vs;
    string s;
    while(getline(cin,s)){
        stringstream ss(s);
        ss >> s;
        if(find(vs.begin(),vs.end(),s) == vs.end()){
            m[s] = true;
            vs.push_back(s);
        }
        if(ss >> s && ss >> s){
            while(ss >> s){
                if(find(vs.begin(),vs.end(),s) == vs.end()){
                    vs.push_back(s);
                }
                m[s] = false;
            }
        }
    }
    for(int i = 0; i<vs.size(); ++i){
        if(m[vs[i]]){
            cout << vs[i] << endl;
            break;
        }
    }
}

struct Node{
    Node* parent;
    vector<Node*> children;
    int weight;
    int subtreeweight;
    string name;
    int childrencount;
    Node(string name, Node* parent, int weight): parent(parent), weight(weight),subtreeweight(-1), name(name), childrencount(0){}
    Node():parent(nullptr), subtreeweight(-1), childrencount(0){}
    int getSubtreeWeight(){
        int sum = weight;
        if(subtreeweight != -1) return subtreeweight;
        else{
            for(int i = 0; i<children.size(); ++i){
                sum += children[i]->getSubtreeWeight();
            }
            subtreeweight = sum;
            return subtreeweight;
        }
    }
};

vector<Node> v;

void second(){
    int n;
    cin >> n;
    int last = 0;
    v.resize(n);
    string s;
    int w;
    getline(cin,s);
    while(getline(cin,s)){
        stringstream ss(s);
        ss >> s;
        ss >> w;
        bool f = false;
        for(int i = 0; i<v.size(); ++i){
            if(v[i].name == s){
                f = true;
                v[i].weight = w;
                while(ss >> s){
                    v[i].childrencount++;
                    bool found = false;
                    for(int j = 0; j<v.size(); ++j){
                        if(v[j].name == s) {found = true; v[i].children.push_back(&v[j]); v[j].parent = &v[i]; break;}
                    }
                    if(!found){
                        v[last].name = s;
                        v[last].parent = &v[i];
                        v[last].weight = 0;
                        v[i].children.push_back(&v[last]);
                        last++;
                    }
                }
                break;
            }
        }
        if(!f){
            v[last].name = s;
            v[last].parent = nullptr;
            v[last].weight = w;
            int insertedId = last;
            last++;
            while(ss >> s){
                v[insertedId].childrencount++;
                bool found = false;
                for(int j = 0; j<v.size(); ++j){
                    if(v[j].name == s) {found = true; v[insertedId].children.push_back(&v[j]); v[j].parent = &v[insertedId]; break;}
                }
                if(!found){
                    v[last].name = s;
                    v[last].parent = &v[insertedId];
                    v[last].weight = 0;
                    v[insertedId].children.push_back(&v[last]);
                    last++;
                }
            }
        }
    }

    for(int i = 0; i<v.size(); ++i){
        v[i].getSubtreeWeight();
    }

    while(true){
        for(int i = 0; i<v.size(); ++i){
            // cout << "______________" << endl;
            // cout << v[i].name <<  " " << v[i].subtreeweight <<" " << v[i].weight << " " << v[i].childrencount << " ";
            // cout << ((v[i].parent == nullptr)?"nullptr":v[i].parent->name) << endl;
            // for(int j = 0; j<v[i].children.size();++j){
            //     cout << v[i].children[j]->name << " " << v[i].children[j]->subtreeweight << " " << v[i].children[j]->weight << endl;
            // }
            if(v[i].childrencount == 0){
                v[i].childrencount = -1;
                int needed = v[i].subtreeweight;
                int wrong = 0;
                //cout << i << endl;
                for(int j = 0; j<v[i].parent->children.size();++j){
                    if(v[i].parent->children[j]->subtreeweight != needed){
                        wrong++;
                    }
                }
                if(wrong == 0){
                    v[i].parent->childrencount = 0;
                    for(int j = 0; j<v[i].parent->children.size();++j){
                        v[i].parent->children[j]->childrencount = -1;
                    }
                }
                else{
                    cout << v[i].parent->name << endl;
                    for(int j = 0; j<v[i].parent->children.size();++j){
                        cout << v[i].parent->children[j]->name << " " << v[i].parent->children[j]->subtreeweight << " " << v[i].parent->children[j]->weight << endl;
                    }
                    return;
                }
            }
        }
        // break;
    }
}

int main(){
    // first();
    second();
    return 0;
}