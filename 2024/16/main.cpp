#include <bits/stdc++.h>

using namespace std;

int main() {
    vector<string> maze;
    string tmp;
    while(getline(cin, tmp)) {
        maze.emplace_back(tmp);
    }

    int N = maze.size();
    int M = maze[0].size();

    // Facing: 0 = E, 1 = N, 2 = W, 3 = S
    int startX, startY, startF = 0;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            if (maze[i][j] == 'S') {
                startX = i;
                startY = j;
            }
        }
    }

    unordered_map<int, pair<int, int>> changes = {{0, {0, 1}}, {1, {-1, 0}}, {2, {0, -1}}, {3, {1, 0}}};

    vector<vector<vector<optional<pair<int, set<pair<int, int>>>>>>> visited(4, vector<vector<optional<pair<int, set<pair<int, int>>>>>>(N, vector<optional<pair<int, set<pair<int, int>>>>>(M, nullopt)));
    priority_queue<tuple<int, int, int, int, optional<tuple<int, int, int>>>> q;
    q.emplace(0, startX, startY, startF, nullopt);

    int result = -1;
    int endX, endY;
    while (true) {
        auto [score, x, y, facing, prev] = q.top();
        q.pop();
        
        set<pair<int, int>> coords;
        if (prev) {
            auto [px, py, pf] = *prev;
            coords = visited[pf][px][py]->second;
        }
        
        if (visited[facing][x][y] != nullopt) {
            auto& v = visited[facing][x][y];
            if (v->first == score) {
                v->second.merge(coords);
            }
            continue;
        }
        coords.emplace(x, y);
        visited[facing][x][y] = make_optional<pair<int, set<pair<int, int>>>>(score, coords);

        if (result != -1 && -score > result) {
            break;
        }

        if (maze[x][y] == 'E') {
            result = -score;
            endX = x;
            endY = y;
        }

        auto [dx, dy] = changes[facing];
        q.emplace(score - 1000, x, y, (facing + 1) % 4, make_tuple(x, y, facing));
        q.emplace(score - 1000, x, y, (facing + 3) % 4, make_tuple(x, y, facing));
        x += dx;
        y += dy;
        if (x >= 0 && x < N && y >= 0 && y < M &&  maze[x][y] != '#') {
            q.emplace(score - 1, x , y, facing, make_tuple(x - dx, y - dy, facing));
        }
    }
    cout << result << endl;
    
    set<pair<int, int>> bests;
    for (int i = 0; i < 4; ++i) {
        if (visited[i][endX][endY].has_value()) {
            bests.merge(visited[i][endX][endY]->second);
        }
    }
    int result2 = bests.size();
    cout << result2 << endl;
}