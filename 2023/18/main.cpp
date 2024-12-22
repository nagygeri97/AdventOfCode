#include <bits/stdc++.h>

using namespace std;

enum Dir{Up, Right, Down, Left};

pair<int, int> offset(Dir dir) {
    switch(dir) {
        case Up: return {-1, 0};
        case Right: return {0, 1};
        case Down: return {1, 0};
        case Left: return {0, -1};
    }
    throw;
}

Dir stringToDir(const string& s) {
    if (s == "R") return Right;
    if (s == "L") return Left;
    if (s == "U") return Up;
    if (s == "D") return Down;
    throw;
}

int64_t area(const vector<pair<Dir, int64_t>>& input) {
    vector<pair<int64_t, int64_t>> corners;
    int64_t x = 0;
    int64_t y = 0;
    optional<Dir> prevDir = nullopt;
    for (const auto& [dir, amt] : input) {
        auto [dx, dy] = offset(dir);
        dx *= amt;
        dy *= amt;

        x += dx;
        y += dy;

        if (prevDir.has_value()) {
            int64_t ox = 0;
            int64_t oy = 0;
            if ((*prevDir == Right && dir == Up) || (*prevDir == Up && dir == Right)) {
                // do nothing
            } else if ((*prevDir == Right && dir == Down) || (*prevDir == Down && dir == Right)) {
                oy = 1;
            } else if ((*prevDir == Down && dir == Left) || (*prevDir == Left && dir == Down)) {
                ox = 1;
                oy = 1;
            } else if ((*prevDir == Left && dir == Up) || (*prevDir == Up && dir == Left)) {
                ox = 1;
            }
            corners.back().first += ox;
            corners.back().second += oy;
        }
        corners.emplace_back(x, y);

        prevDir = dir;
    }
    Dir dir = get<0>(input.back());
    int64_t ox = 0;
    int64_t oy = 0;
    if ((*prevDir == Right && dir == Up) || (*prevDir == Up && dir == Right)) {
        // do nothing
    } else if ((*prevDir == Right && dir == Down) || (*prevDir == Down && dir == Right)) {
        oy = 1;
    } else if ((*prevDir == Down && dir == Left) || (*prevDir == Left && dir == Down)) {
        ox = 1;
        oy = 1;
    } else if ((*prevDir == Left && dir == Up) || (*prevDir == Up && dir == Left)) {
        ox = 1;
    }
    corners.back().first += ox;
    corners.back().second += oy;
    corners.push_back(corners[0]);

    int64_t area = 0;
    for (int64_t i = 1; i < corners.size(); ++i) {
        auto [x1, y1] = corners[i-1];
        auto [x2, y2] = corners[i];
        area += x1 * y2 - x2 * y1;
    }
    area /= 2;
    return abs(area);
}

Dir getDir(char c) {
    if (c == '0') return Right;
    if (c == '1') return Down;
    if (c == '2') return Left;
    if (c == '3') return Up;
    throw;
}

int main() {
    vector<pair<Dir, int64_t>> input;
    vector<pair<Dir, int64_t>> input2;
    string dirStr;
    int64_t amountInput;
    string color;
    while (cin >> dirStr >> amountInput >> color) {
        Dir dir = stringToDir(dirStr);
        input.emplace_back(dir, amountInput);
        input2.emplace_back(getDir(color[color.size() - 2]), stoll(color.substr(2, 5), nullptr, 16));
    }

    cout << area(input) << endl;
    cout << area(input2) << endl;
}