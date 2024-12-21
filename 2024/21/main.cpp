#include <bits/stdc++.h>

using namespace std;

const vector<vector<int>> numPadVec = {
    { 7,  8,  9},
    { 4,  5,  6},
    { 1,  2,  3},
    {-1,  0, 10}
};

enum KeyPadKey{Up, Left, Down, Right, A, Invalid = -1};
const vector<vector<KeyPadKey>> keyPadVec = {
    {Invalid, Up, A},    // - ^ A
    {Left, Down, Right}  // < v >
};

template <typename T>
bool inRange(int x, int y, const vector<vector<T>> vec) {
    if (x < 0 || y < 0 || x >= vec.size() || y >= vec[0].size()) return false;
    if (vec[x][y] < 0) return false;
    return true;
}

KeyPadKey fromOffset(int dx, int dy) {
    static map<pair<int, int>, KeyPadKey> offsetToKeyMap = {
        {{-1, 0}, Up},
        {{1, 0}, Down},
        {{0, -1}, Left},
        {{-1, 0}, Right}
    };
    if (offsetToKeyMap.contains({dx, dy})) {
        return offsetToKeyMap[{dx, dy}];
    }
    throw runtime_error("Invalid offset");
}

pair<int, int> keyToOffset(KeyPadKey key) {
    static vector<pair<int, int>> keyToOffsetMap = {{-1, 0}, {0, -1}, {1, 0}, {0, 1}};
    return keyToOffsetMap.at(key);
}

string keyPadKeyToStr(KeyPadKey key) {
    static vector<string> keyToStrMap = {"^", "<", "v", ">", "A"};
    return keyToStrMap.at(key);
}

int charToButton(char c) {
    if (c == 'A') return 10;
    else return c - '0';
}

class KeyPad {
public:
    virtual int64_t push(KeyPadKey key) = 0;
    virtual shared_ptr<KeyPad> clone() = 0;
    virtual void reset() = 0;

    virtual ~KeyPad() = default;

    int x;
    int y;
};

class HumanKeyPad : public KeyPad {
public:
    HumanKeyPad() {};
    int64_t push(KeyPadKey key) override {
        assert(key != Invalid);
        return 1;
    }
    shared_ptr<KeyPad> clone() override {
        return make_shared<HumanKeyPad>();
    }
    void reset() override {}
};

class RobotKeyPad : public KeyPad {
public:
    RobotKeyPad(shared_ptr<KeyPad> keyPad, int x = 0, int y = 2, shared_ptr<map<tuple<int, int, KeyPadKey>, int64_t>> cache = nullptr) : keyPad(keyPad), x(x), y(y) {
        if (cache == nullptr) {
            this->cache = make_shared<map<tuple<int, int, KeyPadKey>, int64_t>>(); 
        } else {
            this->cache = cache;
        }
    }
    int64_t push(KeyPadKey key) override {
        int targetX = 0;
        int targetY = 0;

        for (int i = 0; i < keyPadVec.size(); ++i) {
            for (int j = 0; j < keyPadVec[0].size(); ++j) {
                if (keyPadVec[i][j] == key) {
                    targetX = i;
                    targetY = j;
                    i = keyPadVec.size();
                    break;
                }
            }
        }

        if (cache->contains({x, y, key})) {
            auto result = (*cache)[{x, y, key}];
            x = targetX;
            y = targetY;
            return result;
        }

        vector<KeyPadKey> usableKeys;
        if (targetX < x) {
            usableKeys.push_back(Up);
        } else if (targetX > x) {
            usableKeys.push_back(Down);
        }

        if (targetY < y) {
            usableKeys.push_back(Left);
        } else if (targetY > y) {
            usableKeys.push_back(Right);
        }

        priority_queue<tuple<int64_t, int, int, bool, shared_ptr<KeyPad>>> q;
        q.emplace(0, x, y, false, keyPad->clone());
        while (!q.empty()) {
            auto [cost, cx, cy, pushed, kp] = q.top();
            q.pop();
            if (cx == targetX && cy == targetY && pushed) {
                (*cache)[{x, y, key}] = -cost;
                x = cx;
                y = cy;
                keyPad = kp;
                return -cost;
            }

            if (cx == targetX && cy == targetY) {
                auto cloneKp = kp->clone();
                auto extraCost = cloneKp->push(A);
                q.emplace(cost - extraCost, cx, cy, true, cloneKp);
            } else {
                for (auto key : usableKeys) {
                    auto [dx, dy] = keyToOffset(key);
                    int nx = cx + dx;
                    int ny = cy + dy;
                    if (inRange(nx, ny, keyPadVec)) {
                        auto cloneKp = kp->clone();
                        auto extraCost = cloneKp->push(key);
                        q.emplace(cost - extraCost, nx, ny, false, cloneKp);
                    }
                }
            }

        }
        throw runtime_error("Failed to push button");
    }
    shared_ptr<KeyPad> clone() override {
        return make_shared<RobotKeyPad>(keyPad->clone(), x, y, cache);
    }
    void reset() override {
        keyPad->reset();
        x = 0;
        y = 2;
    }

    shared_ptr<KeyPad> keyPad;
    int x;
    int y;
    shared_ptr<map<tuple<int, int, KeyPadKey>, int64_t>> cache;
};

class NumPad {
public:
    NumPad(shared_ptr<KeyPad> keyPad) : x(3), y(2), keyPad(keyPad) {}
    void reset() {
        x = 3;
        y = 2;
        keyPad->reset();
    }
    int64_t push(int button) {
        int targetX = 0;
        int targetY = 0;
        for (int i = 0; i < numPadVec.size(); ++i) {
            for (int j = 0; j < numPadVec[0].size(); ++j) {
                if (numPadVec[i][j] == button) {
                    targetX = i;
                    targetY = j;
                    i = numPadVec.size();
                    break;
                }
            }
        }

        vector<KeyPadKey> usableKeys;
        if (targetX < x) {
            usableKeys.push_back(Up);
        } else if (targetX > x) {
            usableKeys.push_back(Down);
        }

        if (targetY < y) {
            usableKeys.push_back(Left);
        } else if (targetY > y) {
            usableKeys.push_back(Right);
        }

        priority_queue<tuple<int64_t, int, int, bool, shared_ptr<KeyPad>>> q;
        q.emplace(0, x, y, false, keyPad->clone());
        while (!q.empty()) {
            auto [cost, cx, cy, pushed, kp] = q.top();
            q.pop();
            if (cx == targetX && cy == targetY && pushed) {
                x = cx;
                y = cy;
                keyPad = kp;
                return -cost;
            }

            if (cx == targetX && cy == targetY) {
                auto cloneKp = kp->clone();
                auto extraCost = cloneKp->push(A);
                q.emplace(cost - extraCost, cx, cy, true, cloneKp);
            } else {
                for (auto key : usableKeys) {
                    auto [dx, dy] = keyToOffset(key);
                    int nx = cx + dx;
                    int ny = cy + dy;
                    if (inRange(nx, ny, numPadVec)) {
                        auto cloneKp = kp->clone();
                        auto extraCost = cloneKp->push(key);
                        q.emplace(cost - extraCost, nx, ny, false, cloneKp);
                    }
                }
            }

        }
        throw runtime_error("Failed to push button");
    }

    int x;
    int y;
    shared_ptr<KeyPad> keyPad;
};

int main() {
    int64_t result = 0;
    int64_t result2 = 0;
    string code;
    shared_ptr<KeyPad> humanKeyPad = make_shared<HumanKeyPad>();
    shared_ptr<KeyPad> robotKeyPad1 = make_shared<RobotKeyPad>(humanKeyPad);
    shared_ptr<KeyPad> robotKeyPad2 = make_shared<RobotKeyPad>(robotKeyPad1);
    NumPad numPad(robotKeyPad2);

    shared_ptr<KeyPad> nestedKeyPad = make_shared<HumanKeyPad>();
    for (int i = 0; i < 25; ++i) {
        nestedKeyPad = make_shared<RobotKeyPad>(nestedKeyPad);
    }
    NumPad numPad2(nestedKeyPad);


    while (getline(cin, code)) {
        int64_t value = stoll(code.substr(0, 3));
        int64_t totalCost = 0;
        int64_t totalCost2 = 0;
        for (char c : code) {
            int button = charToButton(c);
            totalCost += numPad.push(button);;

            totalCost2 += numPad2.push(button);
        }
        result += totalCost * value;
        result2 += totalCost2 * value;
        numPad.reset();
    }
    cout << result << endl;
    cout << result2 << endl;
}