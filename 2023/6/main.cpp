#include <bits/stdc++.h>

using namespace std;

int main() {
    int tmp;
    string tmpStr;
    vector<int> times;
    vector<int> distances;
    getline(cin, tmpStr);
    stringstream timeSs(tmpStr);
    timeSs.ignore("Time:"s.size());
    while (timeSs >> tmp) times.push_back(tmp);
    
    getline(cin, tmpStr);
    stringstream distSs(tmpStr);
    distSs.ignore("Distance:"s.size());
    while (distSs >> tmp) distances.push_back(tmp);

    int result = 1;
    for (int i = 0; i < times.size(); ++i) {
        int t = times[i];
        int d = distances[i];
        int count = 0;
        for (int i = 0; i <= t; ++i) {
            if ((t-i)*i > d) {
                ++count;
            }
        }
        result *= count;
    }
    cout << result << endl;

    stringstream ss;
    for (auto time : times) {
        ss << time;
    }
    int64_t t;
    ss >> t;
    ss.clear();

    for (auto dist : distances) {
        ss << dist;
    }
    int64_t d;
    ss >> d;

    int64_t result2 = 0;
    for (int64_t i = 0; i <= t; ++i) {
        if ((t-i)*i > d) {
            ++result2;
        }
    }

    cout << result2 << endl;
}