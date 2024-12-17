#include <bits/stdc++.h>

using namespace std;

void solve(const vector<string>& m, int64_t expansion) {
    int64_t N = m.size();
    int64_t M = m[0].size();

    vector<bool> dupedRows(N, false);
    vector<bool> dupedCols(M, false);

    for (int64_t i = 0; i < N; ++i) {
        bool allEmpty = true;
        for (int64_t j = 0; j < M; ++j) {
            allEmpty &= m[i][j] == '.';
        }
        dupedRows[i] = allEmpty;
    }

    for (int64_t j = 0; j < M; ++j) {
        bool allEmpty = true;
        for (int64_t i = 0; i < N; ++i) {
            allEmpty &= m[i][j] == '.';
        }
        dupedCols[j] = allEmpty;
    }

    vector<pair<int64_t, int64_t>> indices;
    int64_t dr = 0;
    for (int64_t i = 0; i < N; ++i) {
        if (dupedRows[i]) {
            ++dr;
            continue;
        }
        int64_t dc = 0;
        for (int64_t j = 0; j < M; ++j) {
            if (dupedCols[j]) {
                ++dc;
                continue;
            }
            if (m[i][j] == '#') {
                indices.emplace_back(i + dr*(expansion - 1), j + dc*(expansion - 1));
            }
        }
    }

    int64_t result = 0;
    for (int64_t i = 0; i < indices.size(); ++i) {
        auto [ax, ay] = indices[i];
        for (int64_t j = i + 1; j < indices.size(); ++j) {
            auto [bx, by] = indices[j];
            result += abs(ax - bx) + abs(ay - by);
        }
    }
    
    cout << result << endl;
}

int main() {
    vector<string> m;
    string tmp;
    while (getline(cin, tmp)) {
        m.emplace_back(tmp);
    }

    solve(m, 2);
    solve(m, 1000000);
}