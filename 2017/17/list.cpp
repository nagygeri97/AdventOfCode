void first(){
	list<int> buff = {0};
	int steps;
	auto it = buff.begin();
	cin >> steps;
	for(int i = 1; i<=2017; ++i){
		for(int j = 0; j<steps; ++j){
			++it;
			if(it == buff.end()){
				it = buff.begin();
			}
		}
		buff.insert(it,i);
		++it;
		if(it == buff.end()){
			it = buff.begin();
		}
	}
	cout << *(++it) << endl;

}