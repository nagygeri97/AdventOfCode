#include <bits/stdc++.h>

using namespace std;

class Rule {
public:
    Rule(int64_t destinationStart, int64_t sourceStart, int64_t length) :
        destinationStart(destinationStart), sourceStart(sourceStart), length(length)
        {}
    bool isApplicable(int64_t source) const {
        return source >= sourceStart && source < sourceStart + length;
    }
    int64_t apply(int64_t source) const {
        return destinationStart + source - sourceStart;
    }
private:
    int64_t destinationStart;
    int64_t sourceStart;
    int64_t length;
    friend class RuleMap;
};

class RuleMap {
public:
    RuleMap() {}
    void addRule(Rule rule) {
        rules.emplace_back(move(rule));
    }
    int64_t applyRules(int64_t source) const {
        for (const auto& rule : rules) {
            if (rule.isApplicable(source)) {
                return rule.apply(source);
            }
        }
        return source;
    }
private:
    vector<Rule> rules;
};

int64_t applyRuleMaps(const vector<RuleMap>& ruleMaps, int64_t source) {
    for (const auto& ruleMap : ruleMaps) {
        source = ruleMap.applyRules(source);
    }
    return source;
}

RuleMap readRuleMap() {
    RuleMap ruleMap;
    string tmp;
    // text line
    getline(cin, tmp);
    while (getline(cin, tmp) && !tmp.empty()) {
        stringstream ss(tmp);
        int64_t destinationStart, sourceStart, length;
        ss >> destinationStart >> sourceStart >> length;
        ruleMap.addRule(Rule(destinationStart, sourceStart, length));
    }
    return ruleMap;
}

int main() {
    vector<int64_t> seeds;
    vector<RuleMap> ruleMaps;

    string tmp;
    int64_t num;
    // seeds
    getline(cin, tmp);
    stringstream ss(tmp);
    ss.ignore(6);
    while (ss >> num) {
        seeds.emplace_back(num);
    }
    getline(cin, tmp);

    while (!cin.eof()) {
        ruleMaps.emplace_back(readRuleMap());
    }

    vector<int64_t> locs;
    locs.reserve(seeds.size());
    for (auto seed : seeds) {
        locs.emplace_back(applyRuleMaps(ruleMaps, seed));
    }

    int64_t result = locs[0];
    for (auto loc : locs) {
        if (loc < result) result = loc;
    }
    cout << result << endl;

    int result2 = numeric_limits<int>::max();
    for (int i = 0; i < seeds.size(); i += 2) {
        // cout << i/2+1 << "/" << seeds.size()/2 << endl;
        for (int j = 0; j < seeds[i+1]; ++j) {
            auto val = applyRuleMaps(ruleMaps, seeds[i] + j);
            if (val < result2) {
                result2 = val;
            }
        }
    }
    cout << result2 << endl;
}