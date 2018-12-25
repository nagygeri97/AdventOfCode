#include <iostream>
#include <vector>

using namespace std;

enum Direction{Up,Right,Down,Left};

struct Cart{
	bool isCart;
	Direction dir;
	int turns;
	bool moved;
	Cart():isCart(false),moved(true){}
	Cart(Direction dir):dir(dir),turns(0),isCart(true),moved(false){}
};

int main(){
	vector<vector<char>> track;
	vector<vector<Cart>> carts;
	string s;
	while(getline(cin,s)){
		vector<char> v;
		vector<Cart> c;
		for(int i = 0; i<s.length(); ++i){
			if(s[i] == '^' || s[i] == 'v'){
				v.push_back('|');
				if(s[i] == '^'){
					c.push_back(Cart(Up));
				}
				else{
					c.push_back(Cart(Down));
				}
			}
			else if(s[i] == '>' || s[i] == '<'){
				v.push_back('-');
				if(s[i] == '>'){
					c.push_back(Cart(Right));
				}
				else{
					c.push_back(Cart(Left));
				}
			}
			else{
				c.push_back(Cart());
				v.push_back(s[i]);
			}
		}
		track.push_back(v);
		carts.push_back(c);
	}

	int n = track.size();
	int m = track[0].size();
	while(true){
		// for(int i = 0; i<n; ++i){
		// 	for(int j = 0; j<m; ++j){
		// 		if(carts[i][j].isCart){
		// 			if(carts[i][j].dir == Up) // cout << (char)'^';
		// 			else if(carts[i][j].dir == Down) // cout << (char)'v';
		// 			else if(carts[i][j].dir == Left) // cout << (char)'<';
		// 			else if(carts[i][j].dir == Right) // cout << (char)'>';
		// 		}
		// 		else{
		// 			// cout << (char)track[i][j];
		// 		}
		// 	}
		// 	// cout << endl;
		// }
		// // cout << endl << endl;
		for(int i = 0; i<n; ++i){
			for(int j = 0; j<m; ++j){
				if(carts[i][j].isCart && !carts[i][j].moved){
					carts[i][j].moved = true;
					switch(carts[i][j].dir){
						case Up:{
							if(carts[i-1][j].isCart){
								// cout << j << "," << i-1 << endl;
								// return 0;
								carts[i-1][j] = Cart();
								carts[i][j] = Cart();
								break;
							}
							carts[i-1][j] = carts[i][j];
							carts[i][j] = Cart();
							if(track[i-1][j] == '/'){
								carts[i-1][j].dir = Right;
							}
							else if(track[i-1][j] == '\\'){
								carts[i-1][j].dir = Left;
							}
							else if(track[i-1][j] == '+'){
								switch(carts[i-1][j].turns % 3){
									case 0: carts[i-1][j].dir = Left; break;
									case 1: carts[i-1][j].dir = Up; break;
									case 2: carts[i-1][j].dir = Right; break;
								}
								carts[i-1][j].turns += 1;
							}
							break;
						}
						case Down:{
							if(carts[i+1][j].isCart){
								// cout << j << "," << i+1 << endl;
								// return 0;
								carts[i+1][j] = Cart();
								carts[i][j] = Cart();
								break;
							}
							carts[i+1][j] = carts[i][j];
							carts[i][j] = Cart();
							if(track[i+1][j] == '/'){
								carts[i+1][j].dir = Left;
							}
							else if(track[i+1][j] == '\\'){
								carts[i+1][j].dir = Right;
							}
							else if(track[i+1][j] == '+'){
								switch(carts[i+1][j].turns % 3){
									case 0: carts[i+1][j].dir = Right; break;
									case 1: carts[i+1][j].dir = Down; break;
									case 2: carts[i+1][j].dir = Left; break;
								}
								carts[i+1][j].turns += 1;
							}
							break;
						}
						case Right:{
							if(carts[i][j+1].isCart){
								// cout << j+1 << "," << i << endl;
								// return 0;
								carts[i][j+1] = Cart();
								carts[i][j] = Cart();
								break;
							}
							carts[i][j+1] = carts[i][j];
							carts[i][j] = Cart();
							if(track[i][j+1] == '/'){
								carts[i][j+1].dir = Up;
							}
							else if(track[i][j+1] == '\\'){
								carts[i][j+1].dir = Down;
							}
							else if(track[i][j+1] == '+'){
								switch(carts[i][j+1].turns % 3){
									case 0: carts[i][j+1].dir = Up; break;
									case 1: carts[i][j+1].dir = Right; break;
									case 2: carts[i][j+1].dir = Down; break;
								}
								carts[i][j+1].turns += 1;
							}
							break;
						}
						case Left:{
							if(carts[i][j-1].isCart){
								// cout << j-1 << "," << i << endl;
								// return 0;
								carts[i][j-1] = Cart();
								carts[i][j] = Cart();
								break;
							}
							carts[i][j-1] = carts[i][j];
							carts[i][j] = Cart();
							if(track[i][j-1] == '/'){
								carts[i][j-1].dir = Down;
							}
							else if(track[i][j-1] == '\\'){
								carts[i][j-1].dir = Up;
							}
							else if(track[i][j-1] == '+'){
								switch(carts[i][j-1].turns % 3){
									case 0: carts[i][j-1].dir = Down; break;
									case 1: carts[i][j-1].dir = Left; break;
									case 2: carts[i][j-1].dir = Up; break;
								}
								carts[i][j-1].turns += 1;
							}
							break;
						}
					}
				}
			}
		}
		int cartsLeft = 0;
		int posX = 0;
		int posY = 0;
		for(int i = 0; i<n; ++i){
			for(int j = 0; j<m; ++j){
				if(carts[i][j].isCart){
					carts[i][j].moved = false;
					cartsLeft += 1;
					posX = i; posY = j;
				}
			}
		}
		if(cartsLeft == 1){
			cout << posY << "," << posX << endl;
			return 0;
		}
	}
}