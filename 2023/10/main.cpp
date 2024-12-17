#include <bits/stdc++.h>

using namespace std;

enum Dir {
    North, East, South, West
};

Dir flip(Dir dir) {
    switch(dir) {
        case North: return South;
        case South: return North;
        case East: return West;
        case West: return East;
    }
    throw runtime_error("No matching dir.");
}

vector<Dir> getDirs(char c) {
    switch(c) {
        case '|': return {North, South};
        case '-': return {East, West};
        case 'L': return {North, East};
        case 'J': return {North, West};
        case '7': return {West, South};
        case 'F': return {East, South};
        case '.': return {};
        case 'S': return {};
    }
    return {};
}

set<Dir> getDirsSet(char c) {
    auto v = getDirs(c);
    set<Dir> res;
    for (auto d : v) res.emplace(d);
    return res;
}

char getCharFromDirSet(const set<Dir> dirSet) {
    string s = "|-LJ7F.";
    for (char c : s) {
        if (getDirsSet(c) == dirSet) {
            return c;
        }
    }
    return '.';
}

optional<Dir> exitDir(Dir enterDir, char c) {
    auto dirs = getDirs(c);
    if (dirs.size() != 2) {
        return nullopt;
    }
    auto enterFrom = flip(enterDir);
    if (dirs[0] == enterFrom) {
        return dirs[1];
    } else if (dirs[1] == enterFrom) {
        return dirs[0];
    }
    return nullopt;
}

pair<int, int> change(Dir moveDir) {
    switch(moveDir) {
        case North: return {-1, 0};
        case South: return {1, 0};
        case East: return {0, 1};
        case West: return {0, -1};
    }
    throw runtime_error("No matching dir.");
}

bool inBounds(int N, int M, int x, int y) {
    return x >= 0 && x < N && y >= 0 && y < M;
}

Dir getStartDir(const vector<string>& m, int N, int M, int x, int y) {
    for (auto dir : {North, East, South, West}){
        auto [dx, dy] = change(dir);
        if (inBounds(N, M, x + dx, y + dy)) {
            if (exitDir(dir, m[x+dx][y+dy]).has_value()) {
                return dir;
            }
        }
    }
    throw runtime_error("Cannot get start dir.");
}

void replaceS(vector<string>& m, int N, int M, int x, int y) {
    set<Dir> dirs; 
    for (auto dir : {North, East, South, West}){
        auto [dx, dy] = change(dir);
        if (inBounds(N, M, x + dx, y + dy)) {
            if (exitDir(dir, m[x+dx][y+dy]).has_value()) {
                dirs.emplace(dir);
            }
        }
    }
    m[x][y] = getCharFromDirSet(dirs);
}

bool isRightTurn(Dir entryDir, char c) {
    auto enterFrom = flip(entryDir); 
    switch(enterFrom) {
        case West: return c == '7';
        case East: return c == 'L';
        case North: return c == 'J';
        case South: return c == 'F';
    }
    return false;
}

bool isLeftTurn(Dir entryDir, char c) {
    auto enterFrom = flip(entryDir); 
    switch(enterFrom) {
        case West: return c == 'J';
        case East: return c == 'F';
        case North: return c == 'L';
        case South: return c == '7';
    }
    return false;
}

vector<pair<int, int>> cwOffset(Dir entryDir, char c) {
    auto enterFrom = flip(entryDir);
    if (c == '-') {
        if (enterFrom == West) return {{1, 0}};
        if (enterFrom == East) return {{-1, 0}};
    }
    if (c == '|') {
        if (enterFrom == North) return {{0, -1}};
        if (enterFrom == South) return {{0, 1}};
    }
    if (c == '7') {
        if (enterFrom == West) return {{1, -1}};
        if (enterFrom == South) return {{-1, 0}, {-1, 1}, {0, 1}};
    }
    if (c == 'J') {
        if (enterFrom == North) return {{-1, -1}};
        if (enterFrom == West) return {{1, 0}, {1, 1}, {0, 1}};
    }
    if (c == 'F') {
        if (enterFrom == South) return {{1, 1}};
        if (enterFrom == East) return {{-1, 0}, {-1, -1}, {0, -1}};
    }
    if (c == 'L') {
        if (enterFrom == East) return {{-1, 1}};
        if (enterFrom == North) return {{1, 0}, {1, -1}, {0, -1}};
    }
    stringstream ss;
    ss << "Cannot get offset, c=" << c << ", entryDir=" << entryDir; 
    throw runtime_error(ss.str());
}

vector<pair<int, int>> ccwOffset(Dir entryDir, char c) {
    auto enterFrom = flip(entryDir);
    if (c == '-') {
        if (enterFrom == West) return {{-1, 0}};
        if (enterFrom == East) return {{1, 0}};
    }
    if (c == '|') {
        if (enterFrom == North) return {{0, 1}};
        if (enterFrom == South) return {{0, -1}};
    }
    if (c == '7') {
        if (enterFrom == West) return {{-1, 0}, {-1, 1}, {0, 1}};
        if (enterFrom == South) return {{1, -1}};
    }
    if (c == 'J') {
        if (enterFrom == North) return {{1, 0}, {1, 1}, {0, 1}};
        if (enterFrom == West) return {{-1, -1}};
    }
    if (c == 'F') {
        if (enterFrom == South) return {{-1, 0}, {-1, -1}, {0, -1}};
        if (enterFrom == East) return {{1, 1}};
    }
    if (c == 'L') {
        if (enterFrom == East) return {{1, 0}, {1, -1}, {0, -1}};
        if (enterFrom == North) return {{-1, 1}};
    }
    stringstream ss;
    ss << "Cannot get offset, c=" << c << ", entryDir=" << entryDir; 
    throw runtime_error(ss.str());
}

vector<pair<int, int>> offsetIntoLoop(Dir entryDir, char c, bool clockwise) {
    if (clockwise) return cwOffset(entryDir, c);
    else return ccwOffset(entryDir, c);
}

void floodFill(vector<vector<char>>& flood, int N, int M, int x, int y) {
    if (!inBounds(N, M, x, y) || flood[x][y] != '.') {
        return;
    }

    queue<pair<int, int>> q;
    q.emplace(x, y);
    flood[x][y] = 'I';

    while (!q.empty()) {
        auto [cx, cy] = q.front();
        q.pop();

        for (auto [dx, dy] : vector<pair<int, int>>{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}) {
            if (inBounds(N, M, cx + dx, cy + dy) && flood[cx + dx][cy + dy] == '.') {
                flood[cx + dx][cy + dy] = 'I';
                q.emplace(cx + dx, cy + dy);
            }
        }
    }
}

int main() {
    string tmp;
    vector<string> m;
    while (getline(cin, tmp)) {
        m.emplace_back(tmp);
    }

    int N = m.size();
    int M = m[0].size();
    int sx, sy;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            if (m[i][j] == 'S') {
                sx = i;
                sy = j;
            }
        }
    }

    int count = 0;
    int x = sx, y = sy;
    replaceS(m, N, M, sx, sy);
    Dir dir = getStartDir(m, N, M, sx, sy);
    int rights = 0;
    int lefts = 0;
    vector<vector<char>> flood(N, vector<char>(M, '.'));
    do {
        auto [dx, dy] = change(dir);
        x += dx;
        y += dy;
        if (isRightTurn(dir, m[x][y])) ++rights;
        if (isLeftTurn(dir, m[x][y])) ++lefts;
        dir = *exitDir(dir, m[x][y]);
        flood[x][y] = 'O';
        ++count;
    } while (x != sx || y != sy);

    int result = count / 2;
    cout << result << endl;

    bool clockwise = rights > lefts;
    x = sx;
    y = sy;
    dir = getStartDir(m, N, M, sx, sy);
    do {
        auto [dx, dy] = change(dir);
        x += dx;
        y += dy;
                
        auto ov = offsetIntoLoop(dir, m[x][y], clockwise);
        for (auto [ox, oy] : ov) {
            floodFill(flood, N, M, x + ox, y + oy);
        }

        dir = *exitDir(dir, m[x][y]);
    } while (x != sx || y != sy);

    int result2 = 0;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            if (flood[i][j] == 'I') {
                ++result2;
            }
        }
    }
    cout << result2 << endl;
}