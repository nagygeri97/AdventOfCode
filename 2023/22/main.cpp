#include <bits/stdc++.h>

using namespace std;

enum Dir{X, Y, Z};

struct Brick {
    int x;
    int y;
    int z;
    Dir dir;
    int len;

    unordered_set<int> supports;
    unordered_set<int> supportedBy;
};

tuple<int, int, int> offset(Dir dir) {
    switch(dir) {
        case X: return {1, 0, 0};
        case Y: return {0, 1, 0};
        case Z: return {0, 0, 1};
    }
    throw;
}

Brick parseBrick(const string& str) {
    regex pattern(R"((\d+),(\d+),(\d+)~(\d+),(\d+),(\d+))");
    smatch matches;
    if (regex_search(str, matches, pattern)) {
        int x1 = stoi(matches[1]);
        int y1 = stoi(matches[2]);
        int z1 = stoi(matches[3]);
        int x2 = stoi(matches[4]);
        int y2 = stoi(matches[5]);
        int z2 = stoi(matches[6]);
        Brick brick;
        brick.x = min(x1, x2);
        brick.y = min(y1, y2);
        brick.z = min(z1, z2);
        if (x1 != x2) {
            brick.dir = X;
            brick.len = abs(x1 - x2) + 1;
        } else if (y1 != y2) {
            brick.dir = Y;
            brick.len = abs(y1 - y2) + 1;
        } else {
            brick.dir = Z;
            brick.len = abs(z1 - z2) + 1;
        }
        return brick;
    }
    throw;
}

using Tower = vector<vector<vector<int>>>;

bool isFreeUnder(const Tower& tower, const Brick& brick, int zPos) {
    auto [dx, dy, _] = offset(brick.dir);
    --zPos;
    int xPos = brick.x;
    int yPos = brick.y;
    if (brick.dir == Z) {
        return tower[xPos][yPos][zPos] == -1;
    }
    for (int i = 0; i < brick.len; ++i) {
        if (tower[xPos][yPos][zPos] != -1) {
            return false;
        }
        xPos += dx;
        yPos += dy;
    }
    return true;
}

void placeBrick(Tower& tower, Brick& brick, int zPos, int idx) {
    auto [dx, dy, dz] = offset(brick.dir);
    brick.z = zPos;
    int xPos = brick.x;
    int yPos = brick.y;
    for (int i = 0; i < brick.len; ++i) {
        tower[xPos][yPos][zPos] = idx;
        xPos += dx;
        yPos += dy;
        zPos += dz;
    }
}

void fillSupports(const Tower& tower, Brick& brick, int idx, vector<Brick>& bricks) {
    int zSize = tower[0][0].size();
    if (brick.dir == Z) {
        int topZ = brick.z + brick.len;
        if (topZ < zSize && tower[brick.x][brick.y][topZ] != -1) {
            bricks[tower[brick.x][brick.y][topZ]].supportedBy.insert(idx);
            brick.supports.insert(tower[brick.x][brick.y][topZ]);
        }
        return;
    }

    auto [dx, dy, _] = offset(brick.dir);
    int topZ = brick.z + 1;
    if (topZ >= zSize) return;
    int xPos = brick.x;
    int yPos = brick.y;
    unordered_set<int> aboveBricks;
    for (int i = 0; i < brick.len; ++i) {
        if (tower[xPos][yPos][topZ] != -1) {
            bricks[tower[xPos][yPos][topZ]].supportedBy.insert(idx);
            brick.supports.insert(tower[xPos][yPos][topZ]);
        }
        xPos += dx;
        yPos += dy;
    }
}

int disintegrate(vector<Brick>& bricks, int idx) {
    auto& brick = bricks[idx];
    int result = 0;
    for (int j : brick.supports) {
        bricks[j].supportedBy.erase(idx);
        if (bricks[j].supportedBy.empty()) {
            ++result;
            result += disintegrate(bricks, j);
        }
    }
    return result;
}

int main() {
    string tmp;
    vector<Brick> bricks;
    int maxX = 0, maxY = 0, maxZ = 0;
    while (getline(cin, tmp)) {
        bricks.emplace_back(parseBrick(tmp));
        maxX = max(maxX, bricks.back().x);
        maxY = max(maxY, bricks.back().y);
        maxZ = max(maxZ, bricks.back().z);
        if (bricks.back().dir == X) {
            maxX = max(maxX, bricks.back().x + bricks.back().len - 1);
        } else if (bricks.back().dir == Y) {
            maxY = max(maxY, bricks.back().y + bricks.back().len - 1);
        } else if (bricks.back().dir == Z) {
            maxZ = max(maxZ, bricks.back().z + bricks.back().len - 1);
        }
    }

    sort(bricks.begin(), bricks.end(), [](const Brick& left, const Brick& right){return left.z < right.z;});
    Tower tower(maxX + 1, vector<vector<int>>(maxY + 1, vector<int>(maxZ + 1, -1)));

    for (int i = 0; i < bricks.size(); ++i) {
        int zPos = bricks[i].z;
        while (zPos > 1 && isFreeUnder(tower, bricks[i], zPos)) {
            --zPos;
        }
        placeBrick(tower, bricks[i], zPos, i);
    }

    int result = 0;
    for (int i = 0; i < bricks.size(); ++i) {
        fillSupports(tower, bricks[i], i, bricks);
    }
    for (int i = 0; i < bricks.size(); ++i) {
        bool canBeRemoved = true;
        for (int j : bricks[i].supports) {
            if (bricks[j].supportedBy.size() == 1) {
                canBeRemoved = false;
                break;
            }
        }
        if (canBeRemoved) {
            ++result;
        }
    }

    cout << result << endl;

    int result2 = 0;
    for (int i = 0; i < bricks.size(); ++i) {
        auto bricksCopy = bricks;
        result2 += disintegrate(bricksCopy, i);    
    }
    cout << result2 << endl;
}