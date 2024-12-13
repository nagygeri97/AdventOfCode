#include <bits/stdc++.h>

using namespace std;

int main() {
    string line;
    int result = 0;
    vector<int> wins;
    while (getline(cin, line)) {
        auto colon = line.find(":");
        auto pos = line.find("|");
        string winningStr = line.substr(colon + 1, pos - colon);
        string myStr = line.substr(pos + 1, string::npos);

        unordered_set<int> winning;
        stringstream wss(winningStr);
        int n;
        while (wss >> n) {
            winning.emplace(n);
        }

        stringstream mss(myStr);
        int score = 1;
        int count = 0;
        while (mss >> n) {
            if (winning.contains(n)) {
                score <<= 1;
                ++count;
            }
        }
        score >>= 1;
        result += score;
        wins.emplace_back(count);
    }

    vector<int> copies(wins.size(), 1);
    for (int i = 0; i < copies.size(); ++i) {
        for (int j = 1; j <= wins[i]; ++j) {
            copies[i + j] += copies[i];
        }
    }
    int result2 = accumulate(copies.begin(), copies.end(), 0);

    cout << result << endl;
    cout << result2 << endl;
}