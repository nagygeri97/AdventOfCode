#include <bits/stdc++.h>

using namespace std;

vector<int> detectCycles(const string& start, unordered_map<string, set<string>> g, int length) {
    // how many other ts in it?
    vector<int> result(length, 0);

    // id, len, ts, visited
    queue<tuple<string, int, int, unordered_set<string>>> q;
    q.emplace(start, 0, 0, unordered_set<string>());
    while (!q.empty()) {
        auto [id, len, ts, visited] = q.front();
        q.pop();

        if (len == length && id == start) {
            ++result[ts-1];
            continue;
        }
        
        if (len > length) break;

        if (visited.contains(id)) continue;
        visited.insert(id);

        for (const auto& nb : g[id]) {
            int nts = ts + (nb[0] == 't' ? 1 : 0);
            q.emplace(nb, len+1, nts, visited);
        }
    }
    return result;
}

vector<unordered_set<string>> BronKerbosch(const unordered_map<string, set<string>>& g, unordered_set<string>& R, set<string> P, set<string> X) {
    if (P.empty() && X.empty()) {
        return {R};
    }
    vector<unordered_set<string>> result;
    auto Pcopy = P;
    for (const auto& v : Pcopy) {
        R.insert(v);
        auto Nv = g.at(v);
        set<string> PNv; 
        set_intersection(P.begin(), P.end(), Nv.begin(), Nv.end(), inserter(PNv, PNv.begin()));

        set<string> XNv;
        set_intersection(X.begin(), X.end(), Nv.begin(), Nv.end(), inserter(XNv, XNv.begin()));
        
        auto res = BronKerbosch(g, R, move(PNv), move(XNv));
        R.erase(v);
        P.erase(v);
        X.insert(v);
        for (auto& r : res) {
            result.emplace_back(move(r));
        }
    }
    return result;
}

int main() {
    string tmp;
    
    unordered_map<string, set<string>> g;
    unordered_set<string> starts;
    set<string> allKeys;

    while (getline(cin, tmp)) {
        string a = tmp.substr(0, 2);
        string b = tmp.substr(3, 2);
        g[a].insert(b);
        g[b].insert(a);
        if (a[0] == 't') starts.insert(a);
        if (b[0] == 't') starts.insert(b);
        allKeys.insert(a);
        allKeys.insert(b);
    }

    const int groupSize = 3;
    vector<int> result(groupSize, 0);
    for (const auto& start : starts) {
        auto res = detectCycles(start, g, groupSize);
        for (int i = 0; i < groupSize; ++i) {
            result[i] += res[i] / 2;
        }
    }

    int result1 = 0;
    for (int i = 0; i < groupSize; ++i) {
        result[i] /= (i + 1);
        result1 += result[i];
    }
    cout << result1 << endl;

    unordered_set<string> R;
    int maxSize = 0;
    unordered_set<string> maxSet;
    for (const auto& r : BronKerbosch(g, R, allKeys, {})) {
        if (r.size() > maxSize) {
            maxSize = r.size();
            maxSet = r;
        }
    }

    vector<string> v;
    for (const string& s : maxSet) {
        v.push_back(s);
    }

    sort(v.begin(), v.end());

    for (int i = 0; i < v.size(); ++i) {
        if (i > 0) cout << ",";
        cout << v[i];
    }
    cout << endl;
}