#include <bits/stdc++.h>

using namespace std;

enum Op{AND, OR, XOR};

bool applyOp(bool left, bool right, Op op) {
    switch(op) {
        case AND: return left && right;
        case OR: return left || right;
        case XOR: return left != right;
    }
    throw;
}

Op opFromString(const string& str) {
    if (str == "AND") return AND;
    if (str == "OR") return OR;
    if (str == "XOR") return XOR;
    throw;
}

struct Rule {
    string result;
    string leftLabel;
    string rightLabel;
    Op op;
    optional<bool> leftValue = nullopt;
    optional<bool> rightValue = nullopt;

    bool apply() {
        assert(leftValue.has_value() && rightValue.has_value());
        return applyOp(*leftValue, *rightValue, op);
    }
};

pair<string, bool> parseInitialValue(const string& str) {
    regex pattern(R"(([a-z0-9]+): (\d))");

    smatch matches;
    if (regex_search(str, matches, pattern)) {
        return {matches[1], matches[2] == "1"};
    }
    throw;
}

shared_ptr<Rule> parseRule(const string& str) {
    regex pattern(R"(([a-z0-9]+) (AND|OR|XOR) ([a-z0-9]+) -> ([a-z0-9]+))");

    smatch matches;
    if (regex_search(str, matches, pattern)) {
        shared_ptr<Rule> rule = make_shared<Rule>();
        rule->leftLabel = matches[1];
        rule->rightLabel = matches[3];
        rule->op = opFromString(matches[2]);
        rule->result = matches[4];
        return rule;
    }
    throw;
}

string opToString(Op op) {
    switch(op) {
        case AND: return "AND";
        case OR: return "OR";
        case XOR: return "XOR";
    }
    throw;
}

void toDot(const unordered_map<string, vector<shared_ptr<Rule>>>& children) {
    cout << "digraph {\n";

    vector<string> lines;
    for (const auto& [name, rules] : children) {
        for (const auto& rule : rules) {
            cout << name << " -> " << rule->result << " [label=\"  " << opToString(rule->op) << " \"]\n";
        }
    }

    cout << "}" << endl;
}

set<string> parents(const string& name, const unordered_map<string, shared_ptr<Rule>>& rules) {
    if (name[0] == 'x' || name[0] == 'y') return {name};

    set<string> result;
    // result.push_back(rules.at(name)->leftLabel);
    // result.push_back(rules.at(name)->rightLabel);
    auto leftRes = parents(rules.at(name)->leftLabel, rules);
    auto rightRes = parents(rules.at(name)->rightLabel, rules);
    for (auto x : leftRes) result.insert(x);
    for (auto x : rightRes) result.insert(x);
    return result;
}

void checkAnomalies(int level, const string& carryPrevName, const unordered_map<string, shared_ptr<Rule>>& rules) {
    if (level > 44) return;
    string x = "x"s + (level < 10 ? "0"s : "") + to_string(level);
    string y = "y"s + (level < 10 ? "0"s : "") + to_string(level);    
    string z = "z"s + (level < 10 ? "0"s : "") + to_string(level);

    string XORname = "";
    string ANDname = "";
    for (const auto& [name, rule] : rules) {
        if ((rule->leftLabel == x && rule->rightLabel == y) || (rule->leftLabel == y && rule->rightLabel == x)) {
            if (rule->op == XOR) {
                XORname = name;
            }
            if (rule->op == AND) {
                ANDname = name;
            }
        }
    }
    if (XORname == "") {
        cout << "SOMETHING WRONG WITH XOR ON LEVEL " << level << endl;
        return;
    }
    
    if (ANDname == "") {
        cout << "SOMETHING WRONG WITH AND ON LEVEL " << level << endl;
        return;
    }

    string XORAndCarryPrevName = "";
    string zRuleName = "";
    for (const auto& [name, rule] : rules) {
        if ((rule->leftLabel == XORname && rule->rightLabel == carryPrevName) || (rule->leftLabel == carryPrevName && rule->rightLabel == XORname)) {
            if (rule->op == AND) {
                XORAndCarryPrevName = name;
            }
            if (rule->op == XOR) {
                zRuleName = name;
            }
        }
    }
    if (XORAndCarryPrevName == "") {
        cout << "SOMETHING WRONG WITH XOR AND CARRY_PREV ON LEVEL " << level << endl;
        cout << "carryPrev=" << carryPrevName << ", XORName=" << XORname << ", ANDName=" << ANDname << endl;
        return;
    }
    if (zRuleName == "") {
        cout << "SOMETHING WRONG WITH Z RULE ON LEVEL " << level << endl;
        cout << "carryPrev=" << carryPrevName << ", XORName=" << XORname << ", ANDName=" << ANDname << ", XORAndCarryPrev=" << XORAndCarryPrevName << endl;
        return;
    }

    string carryName = "";
    for (const auto& [name, rule] : rules) {
        if ((rule->leftLabel == ANDname && rule->rightLabel == XORAndCarryPrevName) || (rule->leftLabel == XORAndCarryPrevName && rule->rightLabel == ANDname)) {
            if (rule->op == OR) {
                carryName = name;
            }
        }
    }
    if (carryName == "" || (carryName[0] == 'z' && level < 44)) {
        cout << "SOMETHING WRONG WITH CARRY ON LEVEL " << level << endl;
        cout << "carryPrev=" << carryPrevName << ", XORName=" << XORname << ", ANDName=" << ANDname << ", XORAndCarryPrev=" << XORAndCarryPrevName << endl;
        return;
    }

    checkAnomalies(level + 1, carryName, rules);
}

int main() {
    unordered_map<string, bool> initialValues;
    unordered_map<string, vector<shared_ptr<Rule>>> children;
    unordered_map<string, shared_ptr<Rule>> rules;
    map<int, bool> zValues;

    string tmp;
    while (getline(cin, tmp)) {
        if (tmp.empty()) break;
        auto [name, value] = parseInitialValue(tmp);
        initialValues[name] = value;
    }

    while (getline(cin, tmp)) {
        auto rule = parseRule(tmp);
        rules[rule->result] = rule;
        children[rule->leftLabel].push_back(rule);
        children[rule->rightLabel].push_back(rule);
    }

    queue<pair<string, bool>> q;
    for (const auto& [name, value] : initialValues) {
        q.emplace(name, value);
    }

    while (!q.empty()) {
        auto [name, value] = q.front();
        q.pop();

        if (name[0] == 'z') {
            zValues[stoi(name.substr(1))] = value;
        }

        for (auto rule : children[name]) {
            if (rule->leftLabel == name) {
                rule->leftValue = value;
            } else {
                rule->rightValue = value;
            }

            if (rule->leftValue.has_value() && rule->rightValue.has_value()) {
                bool ruleResult = rule->apply();
                q.emplace(rule->result, ruleResult);
            }
        }
    }

    int64_t result = 0;
    for (auto it = zValues.rbegin(); it != zValues.rend(); ++it) {
        result *= 2;
        result += it->second ? 1 : 0;
    }
    cout << result << endl;

    // toDot(children);
    // for (int i = 0; i < zValues.size(); ++i) {
    //     string name = "z"s + (i<10 ? "0"s : ""s) + to_string(i);
    //     cout << name << ": ";
    //     auto pars = parents(name, rules);
    //     for (auto p : pars) {
    //         cout << p << ", ";
    //     }
    //     cout << endl;
    // }
    // z08 <-> cdj

    // level 0 carry name:
    string carryName = "gsv";
    checkAnomalies(1, carryName, rules);
    // Level  8: z08, cdj
    // Level 16: z16, mrb
    // Level 32: z32, gfm
    // Level 38: dhm, qjd

    vector<string> fixes {"z08", "cdj", "z16", "mrb", "z32", "gfm", "dhm", "qjd"};
    sort(fixes.begin(), fixes.end());
    for (int i = 0; i < fixes.size(); ++i) {
        if (i != 0) cout << ",";
        cout << fixes[i];
    }
    cout << endl;
}
