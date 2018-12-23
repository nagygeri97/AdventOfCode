#include <iostream>
#include <vector>

using namespace std;

struct Elem{
	Elem* next;
	Elem* prev; 
	int value;
	bool isCurrent;
	Elem(int value): value(value), next(nullptr), prev(nullptr), isCurrent(false){};
	~Elem(){
		if(next != nullptr && !next->isCurrent){
			delete next;
		}
	}
	void insertRight(Elem* elem){
		elem->prev = this;
		elem->next = next;
		next->prev = elem;
		next = elem;
	}
	void remove(){
		//cout << "removing " << value << endl;
		next->prev = prev;
		prev->next = next;
		next = nullptr;
		prev = nullptr;
		delete this;
	}
};

Elem* stepRight(Elem* elem, int n){
	if(n == 0) return elem;
	else return stepRight(elem->next, n-1);
}

Elem* stepLeft(Elem* elem, int n){
	if(n == 0) return elem;
	else return stepLeft(elem->prev, n-1);
}

uint64_t scores[1000];

int main(){
	Elem* current = new Elem(0);
	current->isCurrent = true;
	current->next = current;
	current->prev = current;

	int numPlayers, numMarbles;
	
	cin >> numPlayers >> numMarbles;
	int currPlayer = 0;
	for(int currMarble = 1; currMarble <= numMarbles; ++currMarble){
		//cout << "start " << currMarble << endl;
		if(currMarble % 23 == 0){
			//cout << "multiple of 23" << endl;
			//cout << "currPlayer is: " << currPlayer << endl;
			//cout << "numPlayers is: " << numPlayers << endl;
			scores[currPlayer] += currMarble;
			current->isCurrent = false;
			//cout << "before 7 stepLeft current is: " << current->value << endl;
			current = stepLeft(current, 7);
			//cout << "after 7 stepLeft current is: " << current->value << endl;
			scores[currPlayer] += current->value;
			Elem* oldCurrent = current;
			current = stepRight(current, 1);
			//cout << "after step current is: " << current->value << endl;
			current->isCurrent = true;
			//cout << "before remove OldCurrent is: " << oldCurrent->value << endl;
			oldCurrent->remove();
		}
		else{
			//cout << "not multiple of 23" << endl;
			current->isCurrent = false;
			//cout << "before step current is: " << current->value << endl; 
			current = stepRight(current, 1);
			//cout << "before new current is: " << current->value << endl; 
			current->insertRight(new Elem(currMarble));
			current = stepRight(current, 1);
			//cout << "after insert current is: " << current->value << endl;
			current->isCurrent = true;
		}
		//cout << "end" << endl;
		currPlayer += 1; currPlayer %= numPlayers;
	}

	int maxId = 0; uint64_t maxVal = 0;
	for(int i = 0; i<numPlayers; ++i){
		if(scores[i] > maxVal){
			maxId = i;
			maxVal = scores[i];
		}
	}
	cout << maxId + 1 << " has the most score with " << maxVal << " pts" << endl;
}