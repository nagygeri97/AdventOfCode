#include <bits/stdc++.h>

using namespace std;

pair<int, int> score(const vector<vector<int>>& map, int n, int m, int x, int y) {
    vector<vector<bool>> visited(n, vector<bool>(m, false));
    vector<vector<int>> scores(n, vector<int>(m, 0));
    queue<pair<int, int>> q;
    q.emplace(x, y);
    scores[x][y] = 1;

    int result = 0;
    int result2 = 0;
    while (!q.empty()) {
        auto [a, b] = q.front();
        q.pop();

        if (visited[a][b]) continue;
        visited[a][b] = true;

        int curr = map[a][b];
        int currScore = scores[a][b];
        if (curr == 9) {
            ++result;
            result2 += currScore;
            continue;
        }

        if (a-1 >= 0 && map[a-1][b] == curr + 1) {
            q.emplace(a-1, b);
            scores[a-1][b] += currScore;
        }
        if (b-1 >= 0 && map[a][b-1] == curr + 1) {
            q.emplace(a, b-1);
            scores[a][b-1] += currScore;
        }
        if (a+1 < n && map[a+1][b] == curr + 1) {
            q.emplace(a+1, b);
            scores[a+1][b] += currScore;
        }
        if (b+1 < m && map[a][b+1] == curr + 1) {
            q.emplace(a, b+1);
            scores[a][b+1] += currScore;
        }
    }
    return {result, result2};
}

int main() {
    vector<vector<int>> map;
    int n, m;
    string line;
    while(getline(cin, line)) {
        vector<int> row;
        for (char c : line) {
            row.emplace_back(c - '0');
        }
        map.emplace_back(move(row));
    }
    n = map.size();
    m = map[0].size();

    int result = 0;
    int result2 = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (map[i][j] != 0) continue;

            auto [r, r2] = score(map, n, m, i, j);
            result += r;
            result2 += r2;
        }
    }
    cout << result << endl;
    cout << result2 << endl;
}