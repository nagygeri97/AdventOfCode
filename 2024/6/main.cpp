#include <bits/stdc++.h>

using namespace std;

pair<int, int> next(int x, int y, int facing) {
    if (facing == 0) {
        return {x-1, y};
    }
    if (facing == 1) {
        return {x, y+1};
    }
    if (facing == 2) {
        return {x+1, y};
    }
    if (facing == 3) {
        return {x, y-1};
    }
    throw std::range_error("Invalid facing value");
}

bool loops(vector<string> m, int x, int y) {
    int N = m.size();
    int M = m[0].size();
    int facing = 0;
    map<pair<int, int>, unordered_set<int>> cache;
    while (true) {
        if (cache[{x,y}].count(facing) > 0) {
            return true;
        }
        cache[{x,y}].insert(facing);
        auto [nextX, nextY] = next(x, y, facing);
        if (nextX < 0 || nextX >= N || nextY < 0 || nextY >= M) {
            break;
        }
        if (m[nextX][nextY] == '#') {
            ++facing;
            facing %= 4;
        } else {
            x = nextX;
            y = nextY;
        }
    }
    return false;
}

int part2(vector<string> m, int startX, int startY) {
    int N = m.size();
    int M = m[0].size();
    int result = 0;
    for (int i = 0; i < N; ++i) {
        for(int j = 0; j < M; ++j) {
            if (m[i][j] == '.') {
                auto mcpy = m;
                mcpy[i][j] = '#';
                if (loops(mcpy, startX, startY)) {
                    ++result;
                }
            }
        }
    }
    return result;
}

int main() {
    vector<string> m;
    string line;
    int N, M;
    while (getline(cin, line)) {
        m.emplace_back(line);
    }
    N = m.size();
    M = m[0].size();

    int x, y;
    bool done = false;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            if (m[i][j] == '^') {
                 x = i;
                 y = j;
                 done = true;
                 break;
            }
        }
        if (done) {
            break;
        }
    }

    int result2 = part2(m, x, y);

    // Up = 0, Right = 1, Down = 2, Left = 3; 
    int facing = 0;
    while (true) {
        m[x][y] = 'X';
        auto [nextX, nextY] = next(x, y, facing);
        if (nextX < 0 || nextX >= N || nextY < 0 || nextY >= M) {
            break;
        }
        if (m[nextX][nextY] == '#') {
            ++facing;
            facing %= 4;
        } else {
            x = nextX;
            y = nextY;
        }
    }

    int result = 0;
    for (auto& s : m) {
        for (auto c : s) {
            if (c == 'X') {
                ++result;
            }
        }
    }
    cout << result << endl;
    cout << result2 << endl;
}