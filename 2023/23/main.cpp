#include <bits/stdc++.h>

using namespace std;

struct Node {
    Node() {}
    vector<pair<int, int>> next;
    vector<int> nextCost;
    vector<pair<int, int>> prev;
    vector<int> prevCost;
    bool visited = false;
    int mark = 0; // 0 = none, 1 = temp, 2 = perm
    int maxLength = 0;
};

const vector<pair<int, int>> dirs = {{0,1}, {1,0}, {0,-1}, {-1,0}};

bool inRange(const vector<string>& g, int x, int y) {
    return x >= 0 && y >= 0 && x < g.size() && y < g[0].size();
}

bool isPassableInDir(char c, int dx, int dy) {
    if (c == '.') return true;
    if (dx == 1 && dy == 0) {
        return c == 'v';
    }
    if (dx == -1 && dy == 0) {
        return c == '^';
    }
    if (dx == 0 && dy == 1) {
        return c == '>';
    }
    if (dx == 0 && dy == -1) {
        return c == '<';
    }
    throw;
}

bool isSlope(char c) {
    static set<char> m = {'^', 'v', '<', '>'};
    return m.contains(c);
}

bool isJunction(const vector<string>& g, int x, int y) {
    if (g[x][y] != '.') return false;
    int count = 0;
    for (auto [dx, dy] : dirs) {
        if (inRange(g, x + dx, y + dy) && isSlope(g[x + dx][y + dy])) {
            ++count;
        }
    }
    return count >= 3;
}

vector<pair<int, int>> nextCoords(const vector<string>& g, int x, int y) {
    vector<pair<int, int>> result;
    for (auto [dx, dy] : dirs) {
        if (inRange(g, x + dx, y + dy) && isPassableInDir(g[x + dx][y + dy], dx, dy)) {
            result.emplace_back(x + dx, y + dy);
        }
    }
    return result;
}


vector<pair<int, int>> nextCoordsPart2(const vector<string>& g, int x, int y) {
    vector<pair<int, int>> result;
    for (auto [dx, dy] : dirs) {
        if (inRange(g, x + dx, y + dy) && g[x + dx][y + dy] != '#') {
            result.emplace_back(x + dx, y + dy);
        }
    }
    return result;
}

void visit(vector<pair<int, int>>& L, map<pair<int, int>, Node>& nodes, pair<int, int> n) {
    Node& node = nodes[n];
    
    if (node.mark == 2) {
        return;
    }
    if (node.mark == 1) {
        throw;
    }

    node.mark = 1;

    for (const auto& m : node.next) {
        visit(L, nodes, m);
    }

    node.mark = 2;
    L.push_back(n);
}

vector<pair<int, int>> topSort(map<pair<int, int>, Node>& nodes, pair<int, int> start) {
    vector<pair<int, int>> L;
    visit(L, nodes, start);
    reverse(L.begin(), L.end());
    return L;
}

int backtrack(map<pair<int, int>, Node>& nodes, pair<int, int> n, pair<int, int> end) {
    if (n == end) {
        return 0;
    }
    Node& node = nodes[n];

    node.mark = 2;
    int longest = 0;
    for (int i = 0; i < node.next.size(); ++i) {
        if (nodes[node.next[i]].mark != 0) {
            continue;
        }
        longest = max(longest, backtrack(nodes, node.next[i], end) + node.nextCost[i]);
    }
    node.mark = 0;
    return longest;
}

int main() {
    // Idea:
    // It's a DAG (no circles)
    // Find junctions, and costs between junctions by simple (B|D)FS
    // Build simplified DAG from this info
    // Topsort the DAG
    // Find longest path using the topsort

    string tmp;
    vector<string> g;
    while (getline(cin, tmp)) {
        g.push_back(tmp);
    }
    int N = g.size();
    int M = g[0].size();

    int startX = 0;
    int startY;
    for (int i = 0; i < M; ++i) {
        if (g[startX][i] == '.') {
            startY = i;
            break;
        }
    }
    int endX = N - 1;
    int endY;
    for (int i = 0; i < M; ++i) {
        if (g[endX][i] == '.') {
            endY = i;
            break;
        }
    }

    {
        map<pair<int, int>, Node> nodes;
        nodes[{startX, startY}] = Node();

        queue<pair<int, int>> nodesQ;
        nodesQ.emplace(startX, startY);
        while (!nodesQ.empty()) {
            auto [jx, jy] = nodesQ.front();
            nodesQ.pop();

            if (jx == endX && jy == endY) {
                break;
            }

            if (nodes[{jx, jy}].visited) {
                continue;
            }
            nodes[{jx, jy}].visited = true;

            // Start BFS from here
            queue<tuple<int, int, int>> bfsQ;
            vector<vector<bool>> visited(N, vector<bool>(M, false));
            bfsQ.emplace(jx, jy, 0);
            while (!bfsQ.empty()) {
                auto [x, y, d] = bfsQ.front();
                bfsQ.pop();

                if (visited[x][y]) {
                    continue;
                }
                visited[x][y] = true;

                if ((x != jx || y != jy) && (isJunction(g, x, y) || (x == endX && y == endY))) {
                    nodes[{x, y}].prev.emplace_back(jx, jy);
                    nodes[{x, y}].prevCost.emplace_back(d);
                    nodes[{jx, jy}].next.emplace_back(x, y);
                    nodes[{jx, jy}].nextCost.emplace_back(d);
                    nodesQ.emplace(x, y);
                    continue;
                }

                auto nexts = nextCoords(g, x, y);
                for (auto& [nx, ny] : nexts) {
                    bfsQ.emplace(nx, ny, d + 1);
                }
            }
        }

        // for (auto [key, value] : nodes) {
        //     cout << "(" << key.first << ", " << key.second << ") junction: " << endl;
        //     for (int i = 0; i < value.next.size(); ++i) {
        //         cout << "  (" << value.next[i].first << ", " << value.next[i].second << ") - " << value.nextCost[i] << endl;
        //     }
        // }

        auto topSortedNodes = topSort(nodes, {startX, startY});

        for (const auto& n : topSortedNodes) {
            int longest = 0;
            Node& node = nodes[n];
            for (int i = 0; i < node.prev.size(); ++i) {
                longest = max(longest, nodes[node.prev[i]].maxLength + node.prevCost[i]);
            }
            node.maxLength = longest;
        }

        cout << nodes[{endX, endY}].maxLength << endl;
    }


    {
        map<pair<int, int>, Node> nodes;
        nodes[{startX, startY}] = Node();

        queue<pair<int, int>> nodesQ;
        nodesQ.emplace(startX, startY);
        while (!nodesQ.empty()) {
            auto [jx, jy] = nodesQ.front();
            nodesQ.pop();

            if (jx == endX && jy == endY) {
                break;
            }

            if (nodes[{jx, jy}].visited) {
                continue;
            }
            nodes[{jx, jy}].visited = true;

            // Start BFS from here
            queue<tuple<int, int, int>> bfsQ;
            vector<vector<bool>> visited(N, vector<bool>(M, false));
            bfsQ.emplace(jx, jy, 0);
            while (!bfsQ.empty()) {
                auto [x, y, d] = bfsQ.front();
                bfsQ.pop();

                if (visited[x][y]) {
                    continue;
                }
                visited[x][y] = true;

                if ((x != jx || y != jy) && (isJunction(g, x, y) || (x == endX && y == endY))) {
                    nodes[{x, y}].prev.emplace_back(jx, jy);
                    nodes[{x, y}].prevCost.emplace_back(d);
                    nodes[{jx, jy}].next.emplace_back(x, y);
                    nodes[{jx, jy}].nextCost.emplace_back(d);
                    nodesQ.emplace(x, y);
                    continue;
                }

                auto nexts = nextCoordsPart2(g, x, y);
                for (auto& [nx, ny] : nexts) {
                    bfsQ.emplace(nx, ny, d + 1);
                }
            }
        }

        int result2 = backtrack(nodes, {startX, startY}, {endX, endY});
        cout << result2 << endl;
    }
}
