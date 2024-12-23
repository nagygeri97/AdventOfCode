#include <bits/stdc++.h>

using namespace std;

enum Op{LT, GT};
enum Field{X, M, A, S};

struct Rating {
    int x;
    int m;
    int a;
    int s;

    int getField(Field f) const {
        switch(f) {
            case X: return x;
            case M: return m;
            case A: return a;
            case S: return s;
        }
        throw;
    }
};

struct Rule {
    Op op;
    Field field;
    int value;
    string trueName;

    bool alwaysTrue = false;

    optional<string> apply(const Rating& rating) const {
        if (alwaysTrue) return trueName;
        if (op == LT) {
            return rating.getField(field) < value ? make_optional(trueName) : nullopt;
        } else if (op == GT) {
            return rating.getField(field) > value ? make_optional(trueName) : nullopt;
        }
        throw;
    }
};

struct Workflow {
    string name;
    vector<Rule> rules;

    string applyRules(const Rating& rating) const {
        for (const auto& rule : rules) {
            auto res = rule.apply(rating);
            if (res.has_value()) {
                return *res;
            }
        }
        throw;
    }
};

Op strToOp(const string& str) {
    if (str == "<") return LT;
    if (str == ">") return GT;
    throw;
}

Field strToField(const string& str) {
    if (str == "x") return X;
    if (str == "m") return M;
    if (str == "a") return A;
    if (str == "s") return S;
    throw;
}

Rule parseRule(const string& ruleStr) {
    regex pattern(R"(^([a-zA-z]+|([xmas])([<>])(\d+):([a-zA-Z]+))$)");
    smatch matches;
    Rule rule;
    if (regex_search(ruleStr, matches, pattern)) {
        if (matches[2].length() == 0) {
            rule.trueName = matches[1];
            rule.alwaysTrue = true; 
        } else {
            rule.trueName = matches[5];
            rule.op = strToOp(matches[3]);
            rule.field = strToField(matches[2]);
            rule.value = stoi(matches[4]);
        }
    }
    return rule;
}

vector<Rule> parseRules(const string& rulesStr) {
    stringstream ss(rulesStr);
    string ruleStr;
    vector<Rule> rules;
    while (getline(ss, ruleStr, ',')) {
        rules.push_back(parseRule(ruleStr));
    }
    return rules;
}

Workflow parseWorkflow(const string& wfStr) {
    regex pattern(R"(^(.*)\{(.*)\}$)");
    
    smatch matches;
    Workflow wf;
    if (regex_search(wfStr, matches, pattern)) {
        wf.name = matches[1];
        wf.rules = parseRules(matches[2]);
    }
    return wf;
}

Rating parseRating(const string& ratingStr) {
    regex pattern(R"(^\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}$)");

    smatch matches;
    Rating rating;
    if (regex_search(ratingStr, matches, pattern)) {
        rating.x = stoi(matches[1]);
        rating.m = stoi(matches[2]);
        rating.a = stoi(matches[3]);
        rating.s = stoi(matches[4]);
    }
    return rating;
}

bool applyWfs(const unordered_map<string, Workflow>& wfs, const Rating& rating, const string& currentName) {
    if (currentName == "A") return true;
    if (currentName == "R") return false;

    auto nextName = wfs.at(currentName).applyRules(rating);
    return applyWfs(wfs, rating, nextName);
}

struct Range {
    int64_t xMin = 1;
    int64_t xMax = 4000;
    int64_t mMin = 1;
    int64_t mMax = 4000;
    int64_t aMin = 1;
    int64_t aMax = 4000;
    int64_t sMin = 1;
    int64_t sMax = 4000;

    int64_t count() const {
        return (xMax - xMin + 1) *
               (mMax - mMin + 1) *
               (aMax - aMin + 1) *
               (sMax - sMin + 1);
    }

    int64_t& maxField(Field f) {
        switch(f) {
            case X: return xMax;
            case M: return mMax;
            case A: return aMax;
            case S: return sMax;
        }
        throw;
    }

    int64_t& minField(Field f) {
        switch(f) {
            case X: return xMin;
            case M: return mMin;
            case A: return aMin;
            case S: return sMin;
        }
        throw;
    }
};

int64_t applyWfsWithRanges(const unordered_map<string, Workflow>& wfs, const Range& range, const string& currentName) {
    if (currentName == "A") {
        return range.count();
    }
    if (currentName == "R") {
        return 0;
    }

    const auto& wf = wfs.at(currentName);

    unordered_map<string, vector<Range>> ranges;
    Range remaining = range;
    for (const auto& rule : wf.rules) {
        if (rule.alwaysTrue) {
            ranges[rule.trueName].push_back(remaining);
        } else {
            int64_t val = rule.value;

            // True branch
            Range trueRange = remaining;

            int64_t& maxRef = trueRange.maxField(rule.field);
            int64_t& minRef = trueRange.minField(rule.field);

            if (rule.op == LT) {
                maxRef = min(maxRef, val-1);
            } else {
                minRef = max(minRef, val+1);
            }
            ranges[rule.trueName].push_back(trueRange);

            // False branch
            int64_t& remMaxRef = remaining.maxField(rule.field);
            int64_t& remMinRef = remaining.minField(rule.field);
            
            if (rule.op == LT) {
                remMinRef = max(remMinRef, val);
            } else {
                remMaxRef = min(remMaxRef, val);
            }
        }
    }

    int64_t result = 0;
    for (const auto& [nextName, nextRanges] : ranges) {
        for (const auto& nextRange: nextRanges) {
            result += applyWfsWithRanges(wfs, nextRange, nextName);
        }
    } 
    return result;
}

int main() {
    string tmp;
    unordered_map<string, Workflow> wfs;
    while (getline(cin, tmp)) {
        if (tmp.empty()) break;

        auto wf = parseWorkflow(tmp);
        wfs[wf.name] = wf;
    }

    int result = 0;
    while (getline(cin, tmp)) {
        auto rating = parseRating(tmp);
        if (applyWfs(wfs, rating, "in")) {
            result += rating.x + rating.m + rating.a + rating.s; 
        } 
    }
    cout << result << endl;

    auto result2 = applyWfsWithRanges(wfs, Range(), "in");
    cout << result2 << endl;
}