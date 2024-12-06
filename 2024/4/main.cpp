#include <bits/stdc++.h>

using namespace std;

int main() {
    vector<string> grid;
    string tmp;
    while(getline(cin, tmp)) {
        grid.push_back(tmp);
    }

    int N = grid.size();
    int M = grid[0].size();
    int result = 0;

    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j) {
            if (grid[i][j] != 'X') continue;

            // Right
            if (j + 3 < M) {
                if (grid[i][j + 1] == 'M' && grid[i][j + 2] == 'A' && grid[i][j + 3] == 'S') {
                    ++result;
                }
            }

            // Left
            if (j - 3 >= 0) {
                if (grid[i][j - 1] == 'M' && grid[i][j - 2] == 'A' && grid[i][j - 3] == 'S') {
                    ++result;
                }
            }

            // Up
            if (i - 3 >= 0) {
                if (grid[i - 1][j] == 'M' && grid[i - 2][j] == 'A' && grid[i - 3][j] == 'S') {
                    ++result;
                }
            }

            // Down
            if (i + 3 < N) {
                if (grid[i + 1][j] == 'M' && grid[i + 2][j] == 'A' && grid[i + 3][j] == 'S') {
                    ++result;
                }
            }

            // Down-Right
            if (i + 3 < N && j + 3 < M) {
                if (grid[i + 1][j + 1] == 'M' && grid[i + 2][j + 2] == 'A' && grid[i + 3][j + 3] == 'S') {
                    ++result;
                }
            }

            // Down-Left
            if (i + 3 < N && j - 3 >= 0) {
                if (grid[i + 1][j - 1] == 'M' && grid[i + 2][j - 2] == 'A' && grid[i + 3][j - 3] == 'S') {
                    ++result;
                }
            }

            // Up-Right
            if (i - 3 >= 0 && j + 3 < M) {
                if (grid[i - 1][j + 1] == 'M' && grid[i - 2][j + 2] == 'A' && grid[i - 3][j + 3] == 'S') {
                    ++result;
                }
            }

            // Up-Left
            if (i - 3 >= 0 && j - 3 >= 0) {
                if (grid[i - 1][j - 1] == 'M' && grid[i - 2][j - 2] == 'A' && grid[i - 3][j - 3] == 'S') {
                    ++result;
                }
            }
        }
    }
    cout << result << endl;

    result = 0;
    for (int i = 1; i < N - 1; ++i) {
        for (int j = 1; j < M - 1; ++j) {
            if (grid[i][j] != 'A') continue;

            bool majorOK = false;
            if (grid[i-1][j-1] == 'M' && grid[i+1][j+1] == 'S') {
                majorOK = true;
            }
            if (grid[i-1][j-1] == 'S' && grid[i+1][j+1] == 'M') {
                majorOK = true;
            }

            bool minorOK = false;
            if (grid[i-1][j+1] == 'M' && grid[i+1][j-1] == 'S') {
                minorOK = true;
            }
            if (grid[i-1][j+1] == 'S' && grid[i+1][j-1] == 'M') {
                minorOK = true;
            }

            if (majorOK && minorOK) {
                ++result;
            }
        }
    }
    cout << result << endl;
}