#include <bits/stdc++.h>

using namespace std;

enum Dir{Up, Right, Down, Left};

bool inRange(const vector<string>& field, int x, int y) {
    return x >= 0 && x < field.size() && y >= 0 && y < field[0].size();
}

pair<int, int> getOffset(Dir dir) {
    switch(dir) {
        case Up: return {-1, 0};
        case Right: return {0, 1};
        case Down: return {1, 0};
        case Left: return {0, -1};
    }
    throw;
}

bool goStraight(char c, Dir d) {
    if (c == '.') return true;
    if (c == '|') return d == Up || d == Down;
    if (c == '-') return d == Left || d == Right;
    return false;
}

Dir mirrorDir(char c, Dir d) {
    if (c == '/') {
        switch(d) {
            case Up: return Right;
            case Right: return Up;
            case Down: return Left;
            case Left: return Down;
        }
    }
    if (c == '\\') {
        switch(d) {
            case Up: return Left;
            case Right: return Down;
            case Down: return Right;
            case Left: return Up;
        }
    }
    throw;
}

vector<Dir> splitDir(Dir d) {
    if (d == Up || d == Down) return {Left, Right};
    return {Up, Down};
}

vector<tuple<int, int, Dir>> getNexts(const vector<string>& field, int x, int y, Dir dir) {
    vector<tuple<int, int, Dir>> result;
    if (goStraight(field[x][y], dir)) {
        auto [dx, dy] = getOffset(dir);
        if (inRange(field, x + dx, y + dy)) {
            result.emplace_back(x + dx, y + dy, dir);
        }
        return result;
    } else if (field[x][y] == '|' || field[x][y] == '-') {
        for (Dir d : splitDir(dir)) {
            auto [dx, dy] = getOffset(d);
            if (inRange(field, x + dx, y + dy)) {
                result.emplace_back(x + dx, y + dy, d);
            }
        }
        return result;
    } else {
        Dir md = mirrorDir(field[x][y], dir);
        auto [dx, dy] = getOffset(md);
        if (inRange(field, x + dx, y + dy)) {
            result.emplace_back(x + dx, y + dy, md);
        }
        return result;
    }
}

int solve(const vector<string>& field, int startX, int startY, Dir startDir) {
    int N = field.size();
    int M = field[0].size();

    vector<vector<vector<bool>>> visited(N, vector<vector<bool>>(M, vector<bool>(4, false)));
    queue<tuple<int, int, Dir>> q;
    q.emplace(startX, startY, startDir);
    while (!q.empty()) {
        auto [x, y, dir] = q.front();
        q.pop();
        if (visited[x][y][dir]) {
            continue;
        }
        visited[x][y][dir] = true;

        vector<tuple<int, int, Dir>> nexts = getNexts(field, x, y, dir);
        for (const auto& t : nexts) {
            q.emplace(t);
        }
    }

    int result = 0;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            if (any_of(visited[i][j].begin(), visited[i][j].end(), [](bool b){return b;})) {
                ++result;
            }
        }
    }
    return result;
}

int main() {
    string tmp;
    vector<string> field;
    while (getline(cin, tmp)) {
        field.emplace_back(tmp);
    }

    cout << solve(field, 0, 0, Right) << endl;

    int maxEnergy = 0;
    for (int i = 0; i < field.size(); ++i) {
        // Left edge
        maxEnergy = max(maxEnergy, solve(field, i, 0, Right));
        // Right edge
        maxEnergy = max(maxEnergy, solve(field, i, field[0].size() - 1, Left));
    }

    for (int j = 0; j < field[0].size(); ++j) {
        // Top edge
        maxEnergy = max(maxEnergy, solve(field, 0, j, Down));
        // Bottom edge
        maxEnergy = max(maxEnergy, solve(field, field.size() - 1, j, Up));
    }
    cout << maxEnergy << endl;
}