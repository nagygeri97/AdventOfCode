#include <iostream>
#include <vector>

using namespace std;

int main(){
	string input; cin >> input;
	// Part 1
	int index = 0;
	int pixelsInLayer = 25*6;
	int zeros = pixelsInLayer + 1, ones = 0, twos = 0;
	int minZeros = pixelsInLayer + 1, result = 0;
	while(index < input.length()){
		if(index % pixelsInLayer == 0){
			if(zeros < minZeros){
				minZeros  = zeros;
				result = ones * twos;
			}
			zeros = 0;
			ones = 0;
			twos = 0;
		}
		if(input[index] == '0') ++zeros;
		else if(input[index] == '1') ++ones;
		else if(input[index] == '2') ++twos;
		++index;
	}
	if(zeros < minZeros){
		minZeros = zeros;
		result = ones * twos;
	}
	cout << result << endl;

	// Part 2
	for(int i = 0; i < pixelsInLayer; ++i){
		int value = 2;
		for(int j = i; j < input.length(); j += pixelsInLayer){
			value = input[j] - '0';
			if(value != 2) break;
		}
		if(value == 1) cout << "@";
		else cout << " ";
		if(i % 25 == 24) cout << "\n";
	}

}