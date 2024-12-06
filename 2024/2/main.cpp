#include <bits/stdc++.h>

using namespace std;

class IgnoredElementVec {
public:
    IgnoredElementVec(shared_ptr<vector<int>> vec, int ignored) : underlying(vec), ignored(ignored) {}
    int size() const { return underlying->size() - 1; }
    int operator[](int n) const {
        if (n < ignored) {
            return (*underlying)[n];
        } else {
            return (*underlying)[n + 1];
        }
    }
private:
    shared_ptr<vector<int>> underlying;
    int ignored;
};

template<typename Vec>
bool isSafe(const Vec& report) {
    if (report.size() <= 1) return true;
    int diff = abs(report[0] - report[1]);
    if (diff < 1 || diff > 3) return false; 

    for (int i = 0; i + 2 < report.size(); ++i) {
        diff = abs(report[i + 1] - report[i + 2]);
        if (diff < 1 || diff > 3) return false;
        if ((report[i] < report[i + 1]) != (report[i + 1] < report[i + 2])) return false;
    }

    return true;
}

bool isSafeTolerant(const vector<int>& report) {
    if (isSafe(report)) return true;

    auto vptr = make_shared<vector<int>>(report);

    for (int i = 0; i < report.size(); ++i) {
        auto ignoredVec = IgnoredElementVec(vptr, i);
        if (isSafe(ignoredVec)) return true;
    }
    return false;
}

int main() {
    vector<vector<int>> reports;
    string s;
    while (getline(cin, s)) {
        stringstream ss(s);
        int x;
        vector<int> report;
        while (ss >> x) {
            report.push_back(x);
        }
        reports.emplace_back(move(report));
    }

    int result = 0;
    for (const auto& report : reports) {
        result += isSafe(report);
    }
    cout << result << endl;

    result = 0;
    for (const auto& report : reports) {
        result += isSafeTolerant(report);
    }
    cout << result << endl;
}