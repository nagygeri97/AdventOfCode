#include <bits/stdc++.h>

using namespace std;

void toDot(const unordered_map<string, vector<string>>& g) {
    cout << "graph {\n";
    for (auto& [left, v] : g) {
        for (auto& right : v) {
            cout << left << " -- " << right << " [tooltip=\"" << left << "/" << right << "\"]\n";
        }
    }
    cout << "}" << endl;
}

int main() {
    srand((unsigned)time(0));
    unordered_map<string, vector<string>> g;
    string tmp;
    while (getline(cin, tmp)) {
        stringstream ss(tmp);
        string first;
        ss >> first;
        first = first.substr(0, first.size() - 1);
        string next;
        while (ss >> next) {
            g[first].push_back(next);
            g[next].push_back(first);
        }
    }

    // toDot(g);

    unordered_map<string, int> colors;
    for (auto& [left, nbs] : g) {
        colors[left] = 0;
    }

    
    // From svg visualization:
    // rmg/fql
    // vph/mfc
    // sfm/vmt

    vector<pair<string, string>> edgesToRemove = {{"rmg"s, "fql"s}, {"vph"s, "mfc"s}, {"sfm"s, "vmt"s}};
    for (const auto& [left, right] : edgesToRemove) {
        g[left].erase(remove(g[left].begin(), g[left].end(), right), g[left].end());
        g[right].erase(remove(g[right].begin(), g[right].end(), left), g[right].end());
    }

    string startNode = colors.begin()->first;

    queue<string> q;
    q.emplace(startNode);

    int count = 0;
    while (!q.empty()) {
        auto node = q.front();
        q.pop();

        if (colors[node] == 1) {
            continue;
        }
        colors[node] = 1;
        ++count;
        for (auto& nb : g[node]) {
            q.push(nb);
        }
    }

    int result = count * (g.size() - count);
    cout << result << endl;
}