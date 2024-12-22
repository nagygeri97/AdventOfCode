#include <bits/stdc++.h>

using namespace std;

enum Dir{Up, Right, Down, Left};

bool inRange(const vector<string>& m, int x, int y) {
    return x >= 0 && y >= 0 && x < m.size() && y < m[0].size();
}

vector<Dir> possibleDirs(Dir dir, int straight) {
    vector<Dir> result;
    if (dir == Right || dir == Left) {
        result.emplace_back(Up);
        result.emplace_back(Down);
    } else {
        result.emplace_back(Right);
        result.emplace_back(Left);
    }

    if (straight < 3) {
        result.emplace_back(dir);
    }
    return result;
}

vector<Dir> possibleDirsUltra(Dir dir, int straight) {
    vector<Dir> result;
    if (straight >= 4) {
        if (dir == Right || dir == Left) {
            result.emplace_back(Up);
            result.emplace_back(Down);
        } else {
            result.emplace_back(Right);
            result.emplace_back(Left);
        }
    }

    if (straight < 10) {
        result.emplace_back(dir);
    }
    return result;
}

pair<int, int> offset(Dir dir) {
    switch(dir) {
        case Up: return {-1, 0};
        case Right: return {0, 1};
        case Down: return {1, 0};
        case Left: return {0, -1};
    }
    throw;
}

char dirToChar(Dir dir) {
    switch(dir) {
        case Up: return '^';
        case Right: return '>';
        case Down: return 'v';
        case Left: return '<';
    }
    throw;
}

int main() {
    vector<string> m;
    string tmp;
    while (getline(cin, tmp)) {
        m.push_back(tmp);
    }

    int N = m.size();
    int M = m[0].size();

    {
        int result = numeric_limits<int>::max();
        for (auto start : {Down, Right}) {
            // x, y, dir, straight
            map<tuple<int, int, Dir, int>, bool> visited;

            priority_queue<tuple<int, int, int, Dir, int>> q;
            q.emplace(0, 0, 0, start, 0);

            int currResult = 0;
            while (!q.empty()) {
                auto [cost, x, y, d, s] = q.top();
                q.pop();

                if (visited[{x, y, d, s}]) {
                    continue;
                }
                visited[{x, y, d, s}] = true;

                if (x == N-1 && y == M-1) {
                    currResult = -cost;
                    break;
                }

                auto pds = possibleDirs(d, s);
                for (auto pd : pds) {
                    auto [dx, dy] = offset(pd);
                    if (inRange(m, x + dx, y + dy)) {
                        int ns = d == pd ? s + 1 : 1;
                        q.emplace(cost - (m[x + dx][y + dy] - '0'), x + dx, y + dy, pd, ns);
                    }
                }
            }

            if (currResult > 0) {
                result = min(result, currResult);
            }
        }
        cout << result << endl;
    }

    {
        int result2 = numeric_limits<int>::max();
        for (auto start : {Down, Right}) {
            // x, y, dir, straight
            map<tuple<int, int, Dir, int>, bool> visited2;

            priority_queue<tuple<int, int, int, Dir, int>> q2;
            q2.emplace(0, 0, 0, start, 0);

            int currResult = 0;
            while (!q2.empty()) {
                auto [cost, x, y, d, s] = q2.top();
                q2.pop();

                if (visited2[{x, y, d, s}]) {
                    continue;
                }
                visited2[{x, y, d, s}] = true;

                if (x == N-1 && y == M-1 && s >= 4) {
                    currResult = -cost;
                    break;
                }

                auto pds = possibleDirsUltra(d, s);
                for (auto pd : pds) {
                    auto [dx, dy] = offset(pd);
                    if (inRange(m, x + dx, y + dy)) {
                        int ns = d == pd ? s + 1 : 1;
                        q2.emplace(cost - (m[x + dx][y + dy] - '0'), x + dx, y + dy, pd, ns);
                    }
                }
            }

            if (currResult > 0) {
                result2 = min(result2, currResult);
            }
        }
        cout << result2 << endl;
    }
}