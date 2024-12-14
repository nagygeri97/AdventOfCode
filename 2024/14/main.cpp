#include <bits/stdc++.h>

using namespace std;

struct Robot {
    int px, py;
    int vx, vy;

    void move(int w, int h, int n) {
        px += n*vx;
        py += n*vy;

        px %= w;
        py %= h;

        if (px < 0) px += w;
        if (py < 0) py += h; 
    }

    /*
    0 1
    2 3
    */
    int quadrant(int w, int h) {
        if (px < w/2) {
            if (py < h/2) {
                return 0;
            } else if (py > h/2) {
                return 2;
            }
        } else if (px > w/2) {
            if (py < h/2) {
                return 1;
            } else if (py > h/2) {
                return 3;
            }
        }
        return -1;
    }
};

Robot readRobot(const string& line) {
    Robot robot;
    stringstream ss(line);
    ss.ignore(2);
    ss >> robot.px;
    ss.ignore(1);
    ss >> robot.py;
    ss.ignore(3);
    ss >> robot.vx;
    ss.ignore(1);
    ss >> robot.vy;
    return robot;
}

void visualize(int w, int h, const vector<Robot>& robots) {
    vector<vector<char>> pic(h, vector<char>(w, '.'));
    for (const auto& robot : robots) {
        pic[robot.py][robot.px] = '#';
    }

    for (const auto& line : pic) {
        for (auto c : line) {
            cout << c;
        }
        cout << "\n";
    }
}

int main() {
    // const int w = 11;
    // const int h = 7;
    const int w = 101;
    const int h = 103;
    const int t = 100;
    string tmp;

    vector<Robot> robots;
    while (getline(cin, tmp)) {
        robots.emplace_back(readRobot(tmp));
    }

    vector<int> qs(4, 0);
    for (auto robot : robots) {
        robot.move(w, h, t);
        auto q = robot.quadrant(w, h);
        if (q != -1) ++qs[q];
    }
    int result = std::accumulate(qs.begin(), qs.end(), 1, [](int left, int right){return left * right;});
    cout << result << endl;

    int result2 = 0;
    for (int k = 0; true; ++k) {
        if (103 * k % 101 == 51) {
            result2 = 33 + 103*k;
            break;
        }
    }

    for (auto& robot : robots) {
        robot.move(w, h, result2);
    }
    visualize(w, h, robots);
    cout << result2 << endl;
}

/*
33 + k*103 = 84 + n*101
103k = 51 + 101n
103k mod 101 = 51
*/