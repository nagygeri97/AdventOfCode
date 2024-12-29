#include <bits/stdc++.h>

using namespace std;


unordered_map<char, pair<int, int>> charToOffset = {{'N', {-1, 0}}, {'S', {1, 0}}, {'E', {0, 1}}, {'W', {0, -1}}};

using Graph = map<pair<int, int>, set<pair<int, int>>>;

struct Regex {
    using Ptr = shared_ptr<Regex>;
    virtual ~Regex() = default;

    virtual void print() const = 0;
    virtual pair<int, int> buildG(Graph& g, int x, int y) const = 0;
};

struct SimpleRegex : public Regex {
    SimpleRegex(string pattern) : pattern(move(pattern)) {}
    string pattern;

    void print() const override {
        cout << pattern;
    }

    pair<int, int> buildG(Graph& g, int x, int y) const override {
        for (char c : pattern) {
            auto [dx, dy] = charToOffset[c];
            g[{x, y}].emplace(x + dx, y + dy);
            g[{x + dx, y + dy}].emplace(x, y);
            x += dx;
            y += dy;
        }
        return {x, y};
    }
};

struct SequenceRegex : public Regex {
    // AND relation
    vector<Regex::Ptr> sequence;

    void print() const override {
        for (const auto& p : sequence) {
            p->print();
        }
    }

    pair<int, int> buildG(Graph& g, int x, int y) const override {
        for (const auto& p : sequence) {
            auto res = p->buildG(g, x, y);
            x = res.first;
            y = res.second;
        }
        return {x, y};
    }
};

struct BranchRegex : public Regex {
    // OR relation
    vector<Regex::Ptr> branches;

    void print() const override {
        cout << "(";
        for (int i = 0; i < branches.size(); ++i) {
            if (i > 0) cout << "|";
            branches[i]->print();
        }
        cout << ")";
    }

    pair<int, int> buildG(Graph& g, int x, int y) const override {
        for (const auto& p : branches) {
            p->buildG(g, x, y);
        }
        return {x, y};
    }
};

Regex::Ptr parseRegex(string_view input);

Regex::Ptr parseSequenceRegex(string_view input) {
    auto res = make_shared<SequenceRegex>();
    auto& seq = res->sequence;

    int start = 0;
    int parenDepth = 0;
    for (int i = 0; i < input.size(); ++i) {
        if (input[i] == '(') {
            ++parenDepth;
            if (parenDepth == 1 && start != i) {
                seq.emplace_back(parseRegex(input.substr(start, i - start)));
                start = i;
            }
        }

        if (input[i] == ')') {
            --parenDepth;
            if (parenDepth == 0) {
                seq.emplace_back(parseRegex(input.substr(start, i - start + 1)));
                start = i + 1;
            }
        }
    }
    if (start != input.size()) {
        seq.emplace_back(parseRegex(input.substr(start, input.size() - start)));
    }
    return res;
}

Regex::Ptr parseBranchRegex(string_view input) {
    auto res = make_shared<BranchRegex>();
    auto& branches = res->branches;

    vector<string_view> svs;
    int start = 1;
    int parenDepth = 0;
    for (int i = 1; i < input.size() - 1; ++i) {
        if (input[i] == '(') {
            ++parenDepth;
        }
        if (input[i] == ')') {
            --parenDepth;
        }

        if (input[i] == '|' && parenDepth == 0) {
            branches.push_back(parseRegex(input.substr(start, i - start)));
            start = i + 1;
        }
    }
    branches.push_back(parseRegex(input.substr(start, input.size() - 1 - start)));
    return res;
}

Regex::Ptr parseRegex(string_view input) {
    if (input[0] == '(') {
        return parseBranchRegex(input);
    } else if (any_of(input.begin(), input.end(), [](char c){return !isalpha(c);})) {
        return parseSequenceRegex(input);
    } else {
        return make_shared<SimpleRegex>(string(input));
    }
}

int main() {
    string input;
    getline(cin, input);
    input = input.substr(1, input.size() - 2);
    int pos = 0;
    auto rx = parseRegex(input);

    // rx->print();
    // cout << endl;

    Graph g;
    rx->buildG(g, 0, 0);

    set<pair<int, int>> visited;
    queue<tuple<int, int, int>> q;
    q.emplace(0, 0, 0);
    int result = 0;
    int result2 = 0;
    while (!q.empty()) {
        auto [x, y, d] = q.front();
        q.pop();

        if (visited.contains({x, y})) {
            continue;
        }
        visited.emplace(x, y);
        result = d;
        if (d >= 1000) {
            ++result2;
        }

        for (const auto& nb : g[{x, y}]) {
            q.emplace(nb.first, nb.second, d + 1);
        }
    }
    cout << result << endl;
    cout << result2 << endl;
}