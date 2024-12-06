#include <bits/stdc++.h>

using namespace std;

std::vector<int> split(const std::string& str, char delimiter) {
    std::vector<int> tokens;
    std::stringstream ss(str);
    std::string token;
    
    while (std::getline(ss, token, delimiter)) {
        tokens.push_back(stoi(token));
    }
    
    return tokens;
}

bool haveCommonElement(const std::set<int>& set1, const std::set<int>& set2) {
    std::vector<int> intersection;
    std::set_intersection(set1.begin(), set1.end(), set2.begin(), set2.end(),
                          std::back_inserter(intersection));
    return !intersection.empty();
}

int main() {
    string tmp;
    bool part1 = true;

    vector<pair<int, int>> rules;
    vector<vector<int>> updates;

    while (getline(cin, tmp)) {
        if (tmp.empty()) {
            part1 = false;
            continue;
        }

        if (part1) {
            auto r = split(tmp, '|');
            rules.emplace_back(r[0], r[1]);
        } else {
            updates.emplace_back(split(tmp, ','));
        }
    }

    // mustBeAfter[a] must come after 'a' in a correct update;
    unordered_map<int, set<int>> mustBeAfter;
    for (auto [a, b] : rules) {
        mustBeAfter[a].insert(b);
    }

    int result = 0;
    int result2 = 0;
    for (auto& u : updates) {
        bool wrong = false;
        set<int> prevs;
        for (int e : u) {
            wrong = haveCommonElement(prevs, mustBeAfter[e]);
            if (wrong) break;
            prevs.insert(e);
        } 
        if (!wrong) {
            result += u[u.size() / 2];
        } else {
            auto comp = [&mustBeAfter](int a, int b){
                if (mustBeAfter[a].count(b) > 0) {
                    return true;
                } else if (mustBeAfter[b].count(a) > 0) {
                    return false;
                } else {
                    return a > b;
                }
            };
            sort(u.begin(), u.end(), comp);
            result2 += u[u.size() / 2];
        }
    }
    cout << result << endl;
    cout << result2 << endl;
}