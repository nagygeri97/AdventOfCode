#include <bits/stdc++.h>

using namespace std;

bool inRange(const vector<string>& garden, int x, int y) {
    return x >= 0 && x < garden.size() && y >= 0 && y < garden[0].size();
}

int key(int x, int y) {
    return 1000*x + y;
}

pair<int, int> unkey(int k) {
    return {k / 1000, k % 1000};
}

vector<pair<int, int>> offsets = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};

int main() {
    vector<string> garden;
    string tmp;
    while (getline(cin, tmp)) {
        garden.push_back(tmp);
    }

    int startX, startY;
    int N = garden.size();
    int M = garden[0].size();
    for (int i = 0; i < N; ++i) {
        bool found = false;
        for (int j = 0; j < M; ++j) {
            if (garden[i][j] == 'S') {
                garden[i][j] = '.';
                startX = i;
                startY = j;
                found = true;
                break;
            }
        }
        if (found) break;
    }

    unordered_set<int> coords;
    coords.insert(key(startX, startY));

    int maxSteps = 64;
    for (int i = 0; i < maxSteps; ++i) {
        unordered_set<int> newCoords;
        for (int coord : coords) {
            auto [x, y] = unkey(coord);
            for (auto [dx, dy] : offsets) {
                int nx = x + dx;
                int ny = y + dy;
                if (inRange(garden, nx, ny) && garden[nx][ny] == '.') {
                    newCoords.insert(key(nx, ny));
                }
            }
        }
        coords = move(newCoords);
    }

    cout << coords.size() << endl;

    set<pair<int, int>> coordsSet = {};
    coordsSet.emplace(startX, startY);

    maxSteps = 26501365;
    assert(N == M);
    vector<int64_t> values;
    for (int i = 0; i < maxSteps; ++i) {
        set<pair<int, int>> newCoords;
        for (auto [x, y] : coordsSet) {
            for (auto [dx, dy] : offsets) {
                int nx = x + dx;
                int ny = y + dy;
                int modx = (nx % N + N) % N;
                int mody = (ny % M + M) % M;
                if (garden[modx][mody] == '.') {
                    newCoords.emplace(nx, ny);
                }
            }
        }
        coordsSet = move(newCoords);
        if ((i + 1) % N == maxSteps % N) {
            values.push_back(coordsSet.size());
            if (values.size() == 3) break;
        }
    }

    int64_t diff = values[2] - values[1];
    const int64_t diffDiff = diff - (values[1] - values[0]);

    int repetitions = maxSteps / N;
    repetitions -= 2;
    int64_t result2 = values[2];
    for (int i = 0; i < repetitions; ++i) {
        diff += diffDiff;
        result2 += diff;
    }
    cout << result2 << endl;
}