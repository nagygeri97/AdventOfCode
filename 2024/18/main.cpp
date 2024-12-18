#include <bits/stdc++.h>

using namespace std;

int shortestPathInFrame(const vector<vector<bool>>& frame) {
    const int N = frame.size(); 
    queue<tuple<int, int, int>> q;
    vector<vector<bool>> visited(N, vector<bool>(N, false));
    q.emplace(0, 0, 0);

    while (!q.empty()) {
        auto [x, y, d] = q.front();
        q.pop();

        if (visited[x][y]) {
            continue;
        }
        visited[x][y] = true;

        if (x == N-1 && y == N-1) {
            return d;
        }
        if (x-1 >= 0 && !frame[x-1][y]) {
            q.emplace(x-1, y, d+1);
        }
        if (y-1 >= 0 && !frame[x][y-1]) {
            q.emplace(x, y-1, d+1);
        }
        if (x+1 < N && !frame[x+1][y]) {
            q.emplace(x+1, y, d+1);
        }
        if (y+1 < N && !frame[x][y+1]) {
            q.emplace(x, y+1, d+1);
        }
    }
    return -1;
}

void printFrame(const vector<vector<bool>>& frame) {
    for (const auto& row : frame) {
        for (const auto& cell : row) {
            cout << (cell ? '#' : '.');
        }
        cout << endl;
    }
}

int main() {
    const int N = 70 + 1;
    const int F = 1024;
    // const int N = 6 + 1;
    // const int F = 12;
    vector<pair<int, int>> coords;
    string tmp;
    while (getline(cin, tmp)) {
        int x, y;
        stringstream ss(tmp);
        ss >> x;
        ss.ignore(1);
        ss >> y;
        coords.emplace_back(y, x);
    }

    vector<vector<bool>> frame(N, vector<bool>(N, false));

    for (int i = 0; i < F; ++i) {
        auto [x, y] = coords[i];
        frame[x][y] = true;
    }

    cout << shortestPathInFrame(frame) << endl;

    frame = vector<vector<bool>>(N, vector<bool>(N, false));
    for (int i = 0; i < coords.size(); ++i) {
        auto [x, y] = coords[i];
        frame[x][y] = true;
        if (shortestPathInFrame(frame) < 0) {
            cout << y << "," << x << endl;
            break;
        }
    }
}