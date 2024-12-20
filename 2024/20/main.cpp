#include <bits/stdc++.h>

using namespace std;

bool inRange(const vector<vector<int>>& track, int x, int y) {
    return x >= 0 && x < track.size() && y >= 0 && y < track[0].size();
}

int main() {
    vector<vector<int>> track;
    string tmp;
    int i = 0;
    int startX, startY, endX, endY;
    while (getline(cin, tmp)) {
        vector<int> row;
        int j = 0;
        for (char c : tmp) {
            if (c == '#') row.emplace_back(-1);
            if (c == '.') row.emplace_back(-2);
            if (c == 'S') {
                row.emplace_back(-2);
                startX = i;
                startY = j;
            }
            if (c == 'E') {
                row.emplace_back(-2);
                endX = i;
                endY = j;
            }
            ++j;
        }
        track.emplace_back(row);
        ++i;
    }

    int dist = 0;
    int x = startX, y = startY;
    while (x != endX || y != endY) {
        track[x][y] = dist;
        ++dist;
        if (inRange(track, x+1, y) && track[x+1][y] == -2) {
            x = x+1;
            continue;
        }
        if (inRange(track, x-1, y) && track[x-1][y] == -2) {
            x = x-1;
            continue;
        }
        if (inRange(track, x, y+1) && track[x][y+1] == -2) {
            y = y+1;
            continue;
        }
        if (inRange(track, x, y-1) && track[x][y-1] == -2) {
            y = y-1;
            continue;
        }
    }
    track[endX][endY] = dist;

    int result = 0;
    int result2 = 0;
    const int cheatLength = 2;
    const int cheatLength2 = 20;
    for (int i = 0; i < track.size(); ++i) {
        for (int j = 0; j < track[0].size(); ++j) {
            if (track[i][j] < 0) continue;
            for (int ic = -cheatLength2; ic <= cheatLength2; ++ic) {
                for (int jc = -cheatLength2 + abs(ic); jc <= cheatLength2 - abs(ic); ++jc) {
                    if (inRange(track, i + ic, j + jc) && track[i + ic][j + jc] > track[i][j]) {
                        int save = track[i + ic][j + jc] - track[i][j] - abs(ic) - abs(jc); 
                        if (save >= 100) {
                            if (abs(ic) + abs(jc) <= cheatLength) ++result;
                            ++result2;
                        }
                    }
                }
            }
        }
    }

    cout << result << endl;
    cout << result2 << endl;
}