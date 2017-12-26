#include <iostream>

//needs large stack: g++ -Wl,--stack,1500000000 main.cpp

using namespace std;

const int max_step = 12656374;
int step = 0;
int t[2*max_step];
int pos = max_step;

void a();
void b();
void c();
void d();
void e();
void f();

int check_sum(){
    int db = 0;
    for(int i = 0; i<2*max_step; ++i){
        db += t[i];
    }
    return db;
}

void a(){
    if(step == max_step){
        cout << check_sum() << endl;
        return;
    }
    if(t[pos] == 0){
        t[pos] = 1;
        ++pos;
        ++step;
        b();
    }
    else if(t[pos] == 1){
        t[pos] = 0;
        --pos;
        ++step;
        c();
    }
    
}
void b(){
    if(step == max_step){
        cout << check_sum() << endl;
        return;
    }
    if(t[pos] == 0){
        t[pos] = 1;
        --pos;
        ++step;
        a();
    }
    else if(t[pos] == 1){
        t[pos] = 1;
        --pos;
        ++step;
        d();
    }
}
void c(){
    if(step == max_step){
        cout << check_sum() << endl;
        return;
    }
    if(t[pos] == 0){
        t[pos] = 1;
        ++pos;
        ++step;
        d();
    }
    else if(t[pos] == 1){
        t[pos] = 0;
        ++pos;
        ++step;
        c();
    }
}
void d(){
    if(step == max_step){
        cout << check_sum() << endl;
        return;
    }
    if(t[pos] == 0){
        t[pos] = 0;
        --pos;
        ++step;
        b();
    }
    else if(t[pos] == 1){
        t[pos] = 0;
        ++pos;
        ++step;
        e();
    }
}
void e(){
    if(step == max_step){
        cout << check_sum() << endl;
        return;
    }
    if(t[pos] == 0){
        t[pos] = 1;
        ++pos;
        ++step;
        c();
    }
    else if(t[pos] == 1){
        t[pos] = 1;
        --pos;
        ++step;
        f();
    }
}
void f(){
    if(step == max_step){
        cout << check_sum() << endl;
        return;
    }
    if(t[pos] == 0){
        t[pos] = 1;
        --pos;
        ++step;
        e();
    }
    else if(t[pos] == 1){
        t[pos] = 1;
        ++pos;
        ++step;
        a();
    }
}

int main(){
    a();
}