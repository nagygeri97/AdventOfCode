#include <bits/stdc++.h>

using namespace std;

int main() {
    unordered_map<char, pair<int, int>> moveMap = {{'<', {0, -1}}, {'>', {0, 1}}, {'^', {-1, 0}}, {'v', {1, 0}}};

    vector<string> m;
    string tmp;
    while (getline(cin, tmp) && !tmp.empty()) {
        m.emplace_back(tmp);
    }

    string moves;
    while (getline(cin, tmp)) {
        moves += tmp;
    }

    int N = m.size();
    int M = m[0].size();

    vector<string> m2;
    for (const auto& line : m) {
        string tmpLine;
        for (char c : line) {
            if (c == '#') {
                tmpLine += "##";
            } else if (c == '.') {
                tmpLine += "..";
            } else if (c == 'O') {
                tmpLine += "[]";
            } else if (c == '@') {
                tmpLine += "@.";
            }
        }
        m2.emplace_back(move(tmpLine));
    }

    int x, y;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            if (m[i][j] == '@') {
                x = i;
                y = j;
            }
        }
    }

    for (char move : moves) {
        auto [dx, dy] = moveMap[move];
        int cx = x + dx;
        int cy = y + dy;
        while (m[cx][cy] == 'O') {
            cx += dx;
            cy += dy;
        }
        if (m[cx][cy] == '#') {
            continue;
        } else {
            m[cx][cy] = 'O';
            m[x][y] = '.';
            x += dx;
            y += dy;
            m[x][y] = '@';
        }
    }

    int result = 0;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            if (m[i][j] == 'O') {
                result += 100*i + j;
            }
        }
    }
    cout << result << endl;

    // Part 2
    m = move(m2);
    N = m.size();
    M = m[0].size();

    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            if (m[i][j] == '@') {
                x = i;
                y = j;
            }
        }
    }

    for (char move : moves) {
        auto [dx, dy] = moveMap[move];
        if (dy != 0) {
            // Horizontal push
            int cy = y + dy;
            while (m[x][cy] == '[' || m[x][cy] == ']') {
                cy += dy;
            }
            if (m[x][cy] == '.') {
                char popped = '@';
                m[x][y] = '.';
                cy = y + dy;
                while (m[x][cy] == '[' || m[x][cy] == ']') {
                    char tmp = m[x][cy];
                    m[x][cy] = popped;
                    popped = tmp;
                    cy += dy;
                }
                m[x][cy] = popped;
                x += dx;
                y += dy;
            }
        } else {
            // Vertical push
            bool canMove = true;
            vector<pair<int, int>> pushedVec;
            set<pair<int, int>> pushed;
            queue<pair<int, int>> nextToPush;
            nextToPush.emplace(x, y);
            while (!nextToPush.empty()) {
                auto [cx, cy] = nextToPush.front();
                nextToPush.pop();
                if (pushed.contains({cx, cy})) {
                    continue;
                }
                pushed.emplace(cx, cy);
                pushedVec.emplace_back(cx, cy);
                int nextCx = cx + dx;
                if (m[nextCx][cy] == '#') {
                    canMove = false;
                    break;
                } else if (m[nextCx][cy] == '[') {
                    nextToPush.emplace(nextCx, cy);
                    nextToPush.emplace(nextCx, cy + 1);
                } else if (m[nextCx][cy] == ']') {
                    nextToPush.emplace(nextCx, cy - 1);
                    nextToPush.emplace(nextCx, cy);
                }
            }

            if (canMove) {
                for (int i = pushedVec.size() - 1; i >= 0; --i) {
                    auto [cx, cy] = pushedVec[i];
                    m[cx + dx][cy] = m[cx][cy];
                    m[cx][cy] = '.';
                }
                x += dx;
                y += dy;
            }
        }       
    }

    int resul2 = 0;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            if (m[i][j] == '[') {
                resul2 += 100*i + j;
            }
        }
    }
    cout << resul2 << endl;

}