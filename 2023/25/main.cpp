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

const string& findUf(const string& str, unordered_map<string, pair<string, int>>& uf) {
    if (uf[str].first == str) {
        return str;
    }
    uf[str].first = findUf(uf[str].first, uf);
    return uf[str].first;
}

void unionUf(const string& left, const string& right, unordered_map<string, pair<string, int>>& uf) {
    const string& p1 = findUf(left, uf);
    const string& p2 = findUf(right, uf);

    if (uf[p1].second < uf[p2].second) {
        uf[p2].first = p1;
    } else if (uf[p1].second > uf[p2].second) {
        uf[p1].first = p2;
    } else {
        uf[p2].first = p1;
        ++uf[p1].second;
    }
}

vector<pair<string, string>> kargers(vector<pair<string, string>>& E, unordered_map<string, pair<string, int>> uf) {
    random_device dev;
    mt19937 mt(dev());
    shuffle(E.begin(), E.end(), mt);

    int last = E.size() - 1;
    int vCount = uf.size();

    while (vCount > 2) {
        const auto& edge = E[last];
        --last;

        const auto& p1 = findUf(edge.first, uf);
        const auto& p2 = findUf(edge.second, uf);

        if (p1 == p2) continue;

        unionUf(p1, p2, uf);
        --vCount;
    }

    int count = 0;
    vector<pair<string, string>> res;
    for (int i = 0; i <= last; ++i) {
        const auto& p1 = findUf(E[i].first, uf);
        const auto& p2 = findUf(E[i].second, uf);
        if (p1 != p2) {
            ++count;
            res.emplace_back(E[i]);
        }
    }

    return res;

}

int main() {
    srand((unsigned)time(0));
    unordered_map<string, vector<string>> g;
    string tmp;
    vector<pair<string, string>> E;
    while (getline(cin, tmp)) {
        stringstream ss(tmp);
        string first;
        ss >> first;
        first = first.substr(0, first.size() - 1);
        string next;
        while (ss >> next) {
            g[first].push_back(next);
            g[next].push_back(first);
            E.emplace_back(first, next);
        }
    }

    // toDot(g);

    unordered_map<string, pair<string, int>> uf;
    for (auto& [left, nbs] : g) {
        uf[left] = {left, 0};
    }

    vector<pair<string, string>> res;
    while (true) {
        res = kargers(E, uf);
        if (res.size() == 3) break;
    }

    // for (const auto& [left, right] : res) {
    //     cout << left << "/" << right << endl;
    // }

    unordered_map<string, int> colors;
    for (auto& [left, nbs] : g) {
        colors[left] = 0;
    }
    // From svg visualization:
    // rmg/fql
    // vph/mfc
    // sfm/vmt

    // vector<pair<string, string>> edgesToRemove = {{"rmg"s, "fql"s}, {"vph"s, "mfc"s}, {"sfm"s, "vmt"s}};
    for (const auto& [left, right] : res) {
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