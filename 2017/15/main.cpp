#include <iostream>

#define MAX 2147483647ULL
#define STEPS 5000000
#define A 16807
#define B 48271
#define MASK 0xFFFF

using namespace std;

typedef unsigned long long int UInt64;

void first(){
	UInt64 a, b;
	int match = 0;
	cin >> a >> b; 
	for(int i = 0; i<STEPS; ++i){
		do{
			a *= A;
			a %= MAX;
		}while(a%4);
		do{
			b *= B;
			b %= MAX;
		}while(b%8);
		if( (a & MASK) == (b & MASK) ){
			++match;
		}

	}
	cout << match << endl;
}

int main(){
	first();
	return 0;
}