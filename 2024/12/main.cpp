#include <bits/stdc++.h>

using namespace std;

int countPerimeter(const vector<string>& v, char letter, int x, int y) {
    int result = 0;
    if (x-1 >= 0 && v[x-1][y] != letter) {
        ++result;
    }
    if (y-1 >= 0 && v[x][y-1] != letter) {
        ++result;
    }
    if (x+1 < v.size() && v[x+1][y] != letter) {
        ++result;
    }
    if (y+1 < v[0].size() && v[x][y+1] != letter) {
        ++result;
    }
    if (x-1 < 0) {
        ++result;
    }
    if (y-1 < 0) {
        ++result;
    }
    if (x+1 >= v.size()) {
        ++result;
    }
    if (y+1 >= v[0].size()) {
        ++result;
    }
    return result;
}

int countCorners(const vector<string>& v, char letter, int x, int y) {
    int result = 0;
    vector<vector<bool>> same(3, vector<bool>(3, false));
    if (x-1 >=0) {
        same[0][1] = v[x-1][y] == letter;
        if (y-1 >= 0) {
            same[0][0] = v[x-1][y-1] == letter;
        }
        if (y+1 < v[0].size()) {
            same[0][2] = v[x-1][y+1] == letter;
        }
    }
    if (y-1 >=0) {
        same[1][0] = v[x][y-1] == letter;
    }
    if (x+1 < v.size()) {
        same[2][1] = v[x+1][y] == letter;
        if (y-1 >= 0) {
            same[2][0] = v[x+1][y-1] == letter;
        }
        if (y+1 < v[0].size()) {
            same[2][2] = v[x+1][y+1] == letter;
        }
    }
    if (y+1 < v[0].size()) {
        same[1][2] = v[x][y+1] == letter;
    }

    /*
    00 01 02
    10 11 12
    20 21 22
    */

    // Convex corners
    if (!same[0][1] && !same[1][0]) ++result;
    if (!same[1][0] && !same[2][1]) ++result;
    if (!same[2][1] && !same[1][2]) ++result;
    if (!same[1][2] && !same[0][1]) ++result;

    // Concave corners
    if (same[0][1] && same[1][0] && !same[0][0]) ++result;
    if (same[1][0] && same[2][1] && !same[2][0]) ++result;
    if (same[2][1] && same[1][2] && !same[2][2]) ++result;
    if (same[1][2] && same[0][1] && !same[0][2]) ++result;

    return result;
}

pair<int, int> floodFill(const vector<string>& v, vector<vector<bool>>& visited, int x, int y) {
    char letter = v[x][y];
    int perimeter = 0;
    int corners = 0;
    int area = 0;

    queue<pair<int, int>> q;
    q.emplace(x, y);
    
    while(!q.empty()) {
        auto [cx, cy] = q.front();
        q.pop();
        if (visited[cx][cy]) continue;
        visited[cx][cy] = true;
        ++area;
        perimeter += countPerimeter(v, letter, cx, cy);
        corners += countCorners(v, letter, cx, cy);
        if (cx-1 >= 0 && v[cx-1][cy] == letter) {
            q.emplace(cx-1, cy);
        }
        if (cy-1 >= 0 && v[cx][cy-1] == letter) {
            q.emplace(cx, cy-1);
        }
        if (cx+1 < v.size() && v[cx+1][cy] == letter) {
            q.emplace(cx+1, cy);
        }
        if (cy+1 < v[0].size() && v[cx][cy+1] == letter) {
            q.emplace(cx, cy+1);
        }
    }
    return {area * perimeter, area * corners};
}

int main() {
    vector<string> v;
    string tmp;
    while (getline(cin, tmp)) {
        v.emplace_back(move(tmp));
    }

    int result = 0;
    int result2 = 0;
    vector<vector<bool>> visited(v.size(), vector<bool>(v[0].size(), false));
    for (int i = 0; i < v.size(); ++i) {
        for (int j = 0; j < v[i].size(); ++j) {
            if (visited[i][j]) continue;

            auto [r1, r2] = floodFill(v, visited, i, j);
            result += r1;
            result2 += r2;
        }
    }

    cout << result << endl;
    cout << result2 << endl;
}