#include <iostream>

int main(){
    int n, s = 0, sb = 0;
    while(std::cin >> n){
        s += n/3 - 2;
        int x = n/3 - 2;
        int y = x;
        while(x/3 - 2 > 0){
            y += x/3 - 2;
            x = x/3 - 2;
        }
        sb += y;
    }
    std::cout << s << "\n" << sb << "\n";

}