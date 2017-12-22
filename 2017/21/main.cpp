#include <iostream>
#include <vector>
#include <utility>
#include <algorithm>
#include <cmath>

using namespace std;


vector<string> patternFrom;
vector<string> patternTo;

int size = 3;
vector<string> oldk = {".#.","..#","###"};

void first(){
	string from,to;
	while(cin >> from >> to){
		//cout << from <<" "<< to << endl;
		patternFrom.push_back(from);
		patternTo.push_back(to);
	}
	vector<vector<string>> decomposed;
	for(int iter = 0; iter<18; ++iter){
		decomposed.clear();
		if(size % 2 == 0){
			for(int j = 0; j<size; j+=2){
				for(int k = 0; k<size; k+=2){
					vector<string> tmp;
					for(int i = j; i<j+2; ++i){
						string s = "";
						for(int ii = k; ii<k+2; ++ii){
							s = s + oldk[i][ii];
						}
						tmp.push_back(s);
					}
					decomposed.push_back(tmp);
				}
			}
		}
		else if(size % 3 == 0){
			for(int j = 0; j<size; j+=3){
				for(int k = 0; k<size; k+=3){
					vector<string> tmp;
					for(int i = j; i<j+3; ++i){
						string s = "";
						for(int ii = k; ii<k+3; ++ii){
							s  = s + oldk[i][ii];
						}
						tmp.push_back(s);
					}
					decomposed.push_back(tmp);
				}
			}
		}

		for(vector<string>& v : decomposed){

			// cout << "dcmp" << endl;
			// for(int i = 0; i<v.size(); ++i){
			// 	cout << v[i] << endl;
			// }
			// cout << "enddcmp" << endl << endl;

			string s = "";
			for(int i = 0; i<v.size(); ++i){
				for(int j = 0; j<v[i].size(); ++j){
					s  = s + v[i][j];
				}
			}
			auto it = find(patternFrom.begin(),patternFrom.end(),s);
			if(it == patternFrom.end()){
				s = "";
				for(int i = 0; i<v.size(); ++i){
					for(int j = 0; j<v[i].size(); ++j){
						s  = s + v[j][i];
					}
				}
				//cout << s << endl;
				it = find(patternFrom.begin(),patternFrom.end(),s);
				if(it == patternFrom.end()){
					s = "";
					for(int i = v.size()-1; i>=0; --i){
						for(int j = 0; j<v[i].size(); ++j){
							s  = s + v[i][j];
						}
					}
					//cout << s << endl;
					it = find(patternFrom.begin(),patternFrom.end(),s);
					if(it == patternFrom.end()){
						s = "";
						for(int i = v.size()-1; i>=0; --i){
							for(int j = 0; j<v[i].size(); ++j){
								s  = s + v[j][i];
							}
						}
						//cout << s << endl;
						it = find(patternFrom.begin(),patternFrom.end(),s);
						if(it == patternFrom.end()){
							s = "";
							for(int i = v.size()-1; i>=0; --i){
								for(int j = v.size()-1; j>=0; --j){
									s  = s + v[i][j];
								}
							}
							//cout << s << endl;
							it = find(patternFrom.begin(),patternFrom.end(),s);
							if(it == patternFrom.end()){
								s = "";
								for(int i = v.size()-1; i>=0; --i){
									for(int j = v.size()-1; j>=0; --j){
										s  = s + v[j][i];
									}
								}
								//cout << s << endl;
								it = find(patternFrom.begin(),patternFrom.end(),s);
								if(it == patternFrom.end()){
									s = "";
									for(int i = 0; i<v.size(); ++i){
										for(int j = v.size()-1; j>=0; --j){
											s  = s + v[i][j];
										}
									}
									//cout << s << endl;
									it = find(patternFrom.begin(),patternFrom.end(),s);
									if(it == patternFrom.end()){
										s = "";
										for(int i = 0; i<v.size(); ++i){
											for(int j = v.size()-1; j>=0; --j){
												s  = s + v[j][i];
											}
										}
										//cout << s << endl;
										it = find(patternFrom.begin(),patternFrom.end(),s);
									}
								}
							}
						}
					}
				}
			}
			int ind = it - patternFrom.begin();
			s = patternTo[ind];
			//cout << ind << " " << s << endl;
			v.push_back("");
			int k = 0;
			for(int i = 0; i<v.size(); ++i){
				v[i] = "";
				for(int j = 0; j<v.size(); ++j){
					v[i] = v[i] + s[k];
					++k;
				}
			}
		}

		int si = sqrt(decomposed.size());
		vector<string> uj;
		uj.resize(si*decomposed[0].size(),"");
		size = si*decomposed[0].size();
		int row = 0;
		for(int i = 0; i<si;++i){
			for(int j = 0; j<si; ++j){
				for(int x = 0; x<decomposed[i*si + j].size(); ++x){
					uj[row+x] = uj[row+x] + decomposed[i*si + j][x];
				}
			}
			row+=decomposed[0].size();
		}


		oldk = uj;
		int db = 0;
		for(int i = 0; i<oldk.size(); ++i){
			cout << oldk[i] << endl;
			for(int j = 0; j<oldk[i].size(); ++j){
				if(oldk[i][j] == '#'){
					++db;
				}
			}
		}
		cout << db << endl;
		cout << endl;
	}
}

int main(){
	first();
	return 0;
}