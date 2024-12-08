#include <bits/stdc++.h>

using namespace std;

int main() {
    string line;
    vector<string> m;
    while (getline(cin, line)) {
        m.emplace_back(line);
    }
    int N = m.size();
    int M = m[0].size();

    unordered_map<char, vector<pair<int, int>>> coords;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            if (m[i][j] != '.') {
                coords[m[i][j]].emplace_back(i, j);
            }
        }
    }

    set<pair<int, int>> antinodes;
    set<pair<int, int>> antinodes2;
    for (auto& [_, v] : coords) {
        for (int i = 0; i < v.size(); ++i) {
            for (int j = i + 1; j < v.size(); ++j) {
                int distX = v[i].first - v[j].first;
                int distY = v[i].second - v[j].second;

                int x1 = v[i].first + distX;
                int y1 = v[i].second + distY;
                if (x1 >= 0 && x1 < N && y1 >= 0 && y1 < M) {
                    antinodes.emplace(x1, y1);
                }

                int x2 = v[j].first - distX;
                int y2 = v[j].second - distY;
                if (x2 >= 0 && x2 < N && y2 >= 0 && y2 < M) {
                    antinodes.emplace(x2, y2);
                }

                int div = gcd(distX, distY);
                if (div != 0) {
                    distX /= div;
                    distY /= div;
                } else if (distX == 0) {
                    distY = 1;
                } else if (distY == 0) {
                    distX = 1;
                }

                int x = v[i].first;
                int y = v[i].second;
                while (x >= 0 && x < N && y >= 0 && y < M) {
                    antinodes2.emplace(x, y);
                    x += distX;
                    y += distY;
                }

                x = v[i].first;
                y = v[i].second;
                while (x >= 0 && x < N && y >= 0 && y < M) {
                    antinodes2.emplace(x, y);
                    x -= distX;
                    y -= distY;
                }
            }
        }
    }
    cout << antinodes.size() << endl;
    cout << antinodes2.size() << endl;
}