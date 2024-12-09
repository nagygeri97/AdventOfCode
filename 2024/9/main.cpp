#include <bits/stdc++.h>

using namespace std;

struct Elem {
    int id;
    int size;
    int free;
};

int main() {
    string line;
    getline(cin, line);
    int64_t result = 0;

    vector<int> fs;
    fs.reserve(line.size() * 9);
    for (int i = 0; i < line.size(); ++i) {
        int id = i % 2 == 0 ? i / 2 : -1;
        int count = line[i] - '0';
        for (int j = 0; j < count; ++j) {
            fs.push_back(id);
        }
    }

    int end = fs.size() - 1;
    for (int i = 0; i < end; ++i) {
        if (fs[i] == -1) {
            while(fs[end] == -1) {
                --end;
            }
            if (end <= i) break;
            fs[i] = fs[end];
            fs[end] = -1;
        }
    }

    for (int i = 0; i < fs.size(); ++i) {
        if (fs[i] >= 0) {
            result += fs[i] * i;
        }
    }

    cout << result << endl;

    fs.clear();
    fs.reserve(line.size() * 9);
    for (int i = 0; i < line.size(); ++i) {
        int id = i % 2 == 0 ? i / 2 : -1;
        int count = line[i] - '0';
        for (int j = 0; j < count; ++j) {
            fs.push_back(id);
        }
    }

    end = fs.size() - 1;
    while (end >= 0) {
        while (fs[end] == -1) {
            --end;
        }

        int currId = fs[end];
        int count = 0;
        while (fs[end] == currId) {
            --end;
            ++count;
        }

        int start = 0;
        int currRun = 0;
        int startCurrRun = -1;
        while (start <= end) {
            if (fs[start] != -1) {
                currRun = 0;
                startCurrRun = -1;
            } else {
                if (startCurrRun == -1) {
                    startCurrRun = start;
                }
                ++currRun;
                if (currRun >= count) {
                    for (int i = 0; i < currRun; ++i) {
                        fs[startCurrRun + i] = currId;
                        fs[end + 1 + i] = -1;
                    }
                    break;
                }
            }
            ++start;
        }
    }

    int64_t result2 = 0;
    for (int i = 0; i < fs.size(); ++i) {
        if (fs[i] >= 0) {
            result2 += fs[i] * i;
        }
    }
    cout << result2 << endl;
}
