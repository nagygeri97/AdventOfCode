#include <iostream>
#include <vector>

using namespace std;

int readNode(int& sum){
	int metaDataCount;
	int childrenCount;
	int value = 0;
	vector<int> childValues;
	cin >> childrenCount >> metaDataCount;
	childValues.resize(childrenCount,0);
	for(int i = 0; i<childrenCount; ++i){
		childValues[i] = readNode(sum);
	}
	int metaData;

	for(int i = 0; i<metaDataCount; ++i){
		cin >> metaData;
		sum += metaData;
		if(childrenCount == 0) value += metaData;
		else{
			if(metaData > childrenCount || metaData < 1) continue;
			value += childValues[metaData-1];
		}
	}
	return value;
}

int main(){
	int sum = 0;
	int value = readNode(sum);
	cout << sum << endl;
	cout << value << endl;
}