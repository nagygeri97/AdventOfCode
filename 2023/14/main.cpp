#include <bits/stdc++.h>

using namespace std;

void rollNorth(vector<string>& table) {
    for (int i = 0; i < table.size(); ++i) {
        for (int j = 0; j < table[0].size(); ++j) {
            if (table[i][j] == 'O') {
                int ii = i-1;
                while (ii >= 0 && table[ii][j] == '.') {
                    table[ii][j] = 'O';
                    table[ii+1][j] = '.';
                    --ii;
                }
            }
        }
    }
}

void rollSouth(vector<string>& table) {
    for (int i = table.size() - 1; i >= 0; --i) {
        for (int j = 0; j < table[0].size(); ++j) {
            if (table[i][j] == 'O') {
                int ii = i+1;
                while (ii < table.size() && table[ii][j] == '.') {
                    table[ii][j] = 'O';
                    table[ii-1][j] = '.';
                    ++ii;
                }
            }
        }
    }
}

void rollWest(vector<string>& table) {
    for (int i = 0; i < table.size(); ++i) {
        for (int j = 0; j < table[0].size(); ++j) {
            if (table[i][j] == 'O') {
                int jj = j-1;
                while (jj >= 0 && table[i][jj] == '.') {
                    table[i][jj] = 'O';
                    table[i][jj+1] = '.';
                    --jj;
                }
            }
        }
    }
}

void rollEast(vector<string>& table) {
    for (int i = 0; i < table.size(); ++i) {
        for (int j = table[0].size() - 1; j >= 0; --j) {
            if (table[i][j] == 'O') {
                int jj = j+1;
                while (jj < table[0].size() && table[i][jj] == '.') {
                    table[i][jj] = 'O';
                    table[i][jj-1] = '.';
                    ++jj;
                }
            }
        }
    }
}

void cycle(vector<string>& table) {
    rollNorth(table);
    rollWest(table);
    rollSouth(table);
    rollEast(table);
}

int64_t getLoad(const vector<string>& table) {
    int64_t result = 0;
    for (int i = 0; i < table.size(); ++i) {
        for (int j = 0; j < table[0].size(); ++j) {
            if (table[i][j] == 'O') {
                result += table.size() - i;
            }
        }
    }
    return result;
}

int main() {
    vector<string> table;
    string tmp;
    while (getline(cin, tmp)) {
        table.emplace_back(tmp);
    }

    auto tableCopy = table;
    rollNorth(tableCopy);
    cout << getLoad(tableCopy) << endl;

    map<vector<string>, int64_t> saved;
    vector<vector<string>> tables;
    saved[table] = 0;
    tables.push_back(table);
    int64_t repLength = 0;
    int64_t offset = 0;
    int64_t maxI = 1000000000;
    for (int i = 1; i <= maxI; ++i) {
        cycle(table);
        if (saved.contains(table)) {
            offset = saved[table];
            repLength = i - offset;
            break; 
        }
        saved[table] = i;
        tables.push_back(table);
    }
    const auto& sameTable = tables[(maxI - offset) % repLength + offset];
    cout << getLoad(sameTable) << endl;
}