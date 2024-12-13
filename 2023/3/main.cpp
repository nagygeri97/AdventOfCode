#include <bits/stdc++.h>

using namespace std;

bool isSymbol(const vector<string>& lines, int x, int y, vector<pair<int, int>>& stars) {
    char c = lines[x][y];
    if (c == '*') {
        stars.emplace_back(x, y);
    }
    return !isdigit(c) && c != '.';
}

int main() {
    vector<string> lines;
    string tmp;
    while (getline(cin, tmp)) {
        lines.emplace_back(tmp);
    }

    int N = lines.size();
    int M = lines[0].size();

    int result = 0;
    int result2 = 0;

    map<pair<int, int>, vector<int>> starmap;

    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            if (isdigit(lines[i][j])) {
                int n = 0;
                bool ok = false;
                vector<pair<int, int>> stars;
                if (j-1 >= 0) {
                    ok |= isSymbol(lines, i, j-1, stars);
                    if (i-1 >= 0) {
                        ok |= isSymbol(lines, i-1, j-1, stars);
                    }
                    if (i+1 < N) {
                        ok |= isSymbol(lines, i+1, j-1, stars);
                    }
                }
                while (j < M && isdigit(lines[i][j])) {
                    n *= 10;
                    n += lines[i][j] - '0';

                    if (i-1 >= 0) {
                        ok |= isSymbol(lines, i-1, j, stars);
                    }
                    if (i+1 < N) {
                        ok |= isSymbol(lines, i+1, j, stars);
                    }

                    ++j;
                }
                if (j < M) {
                    ok |= isSymbol(lines, i, j, stars);
                    if (i-1 >= 0) {
                        ok |= isSymbol(lines, i-1, j, stars);
                    }
                    if (i+1 < N) {
                        ok |= isSymbol(lines, i+1, j, stars);
                    }
                }

                if (ok) {
                    result += n;
                }

                for (auto star : stars) {
                    starmap[star].emplace_back(n);
                }
            }
        }
    }

    for (auto [_, nums] : starmap) {
        if (nums.size() == 2) {
            result2 += nums[0] * nums[1];
        }
    }

    cout << result << endl;
    cout << result2 << endl;
}