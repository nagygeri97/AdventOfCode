#include <bits/stdc++.h>

#include <boost/multiprecision/cpp_int.hpp>

using namespace std;
using namespace boost::multiprecision;

struct Stone {
    int64_t px, py, pz;
    int64_t vx, vy, vz;
};

ostream& operator<<(ostream& stream, const Stone& stone) {
    return stream << stone.px << ", " << stone.py << ", " << stone.pz << " @ " << stone.vx << ", " << stone.vy << ", " << stone.vz;
}

Stone parseStone(const string& str) {
    regex pattern(R"((-?\d+), +(-?\d+), +(-?\d+) +@ +(-?\d+), +(-?\d+), +(-?\d+))");
    smatch matches;
    if (regex_search(str, matches, pattern)) {
        Stone stone;
        stone.px = stoll(matches[1]);
        stone.py = stoll(matches[2]);
        stone.pz = stoll(matches[3]);
        stone.vx = stoll(matches[4]);
        stone.vy = stoll(matches[5]);
        stone.vz = stoll(matches[6]);
        return stone;
    }
    throw;
}

bool isParallel(const Stone& s1, const Stone& s2) {
    int64_t vx1 = s1.vx;
    int64_t vy1 = s1.vy;
    int64_t gcd1 = gcd(vx1, vy1);
    vx1 /= gcd1;
    vy1 /= gcd1;

    int64_t vx2 = s2.vx;
    int64_t vy2 = s2.vy;
    int64_t gcd2 = gcd(vx2, vy2);
    vx2 /= gcd2;
    vy2 /= gcd2;

    return vx1 == vx2 && vy1 == vy2;
}

bool isGoodPair(const Stone& s1, const Stone& s2) {
    static const int64_t minRange = 200000000000000ll;
    static const int64_t maxRange = 400000000000000ll;
    // static const int64_t minRange = 7ll;
    // static const int64_t maxRange = 27ll;

    // cout << s1 << "\n" << s2 << endl;

    if (isParallel(s1, s2)) return false;

    double px1 = static_cast<double>(s1.px);
    double py1 = static_cast<double>(s1.py);
    double px2 = static_cast<double>(s2.px);
    double py2 = static_cast<double>(s2.py);

    double vx1 = static_cast<double>(s1.vx);
    double vy1 = static_cast<double>(s1.vy);
    double vx2 = static_cast<double>(s2.vx);
    double vy2 = static_cast<double>(s2.vy);
    
    double m1 = vy1 / vx1;
    double c1 = -px1 * (vy1 / vx1) + py1;

    double m2 = vy2 / vx2;
    double c2 = -px2 * (vy2 / vx2) + py2;

    double k = (c2 - c1) / (m1 - m2);
    if (k < minRange || k > maxRange) return false;

    double y = m1 * k + c1;
    if (y < minRange || y > maxRange) return false;

    double t1 = (k - px1) / vx1;
    if (t1 < 0) return false;

    double t2 = (k - px2) / vx2;
    if (t2 < 0) return false;

    return true;
}

tuple<int128_t, int128_t, int128_t> cross(const tuple<int128_t, int128_t, int128_t>& left, const tuple<int128_t, int128_t, int128_t>& right) {
    auto [a1, a2, a3] = left;
    auto [b1, b2, b3] = right;

    return {a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1};
}

int128_t dot(const tuple<int128_t, int128_t, int128_t>& left, const tuple<int128_t, int128_t, int128_t>& right) {
    auto [a1, a2, a3] = left;
    auto [b1, b2, b3] = right;

    return a1*b1 + a2*b2 + a3*b3;
}

tuple<int128_t, int128_t, int128_t> scalarMul(const tuple<int128_t, int128_t, int128_t>& vec, int128_t scalar) {
    auto [a, b, c] = vec;
    return {scalar * a, scalar * b, scalar * c};
}

tuple<int128_t, int128_t, int128_t> scalarDiv(const tuple<int128_t, int128_t, int128_t>& vec, int128_t scalar) {
    auto [a, b, c] = vec;
    return {a / scalar, b / scalar, c / scalar};
}

tuple<int128_t, int128_t, int128_t> add(const tuple<int128_t, int128_t, int128_t>& left, const tuple<int128_t, int128_t, int128_t>& right) {
    auto [a1, a2, a3] = left;
    auto [b1, b2, b3] = right;

    return {a1 + b1, a2 + b2, a3 + b3};
}

tuple<int128_t, int128_t, int128_t> sub(const tuple<int128_t, int128_t, int128_t>& left, const tuple<int128_t, int128_t, int128_t>& right) {
    auto [a1, a2, a3] = left;
    auto [b1, b2, b3] = right;

    return {a1 - b1, a2 - b2, a3 - b3};
}

int main() {
    string tmp;
    vector<Stone> stones;
    while (getline(cin, tmp)) {
        stones.emplace_back(parseStone(tmp));
    }

    int64_t result = 0;
    for (int i = 0; i < stones.size(); ++i) {
        for (int j = i+1; j < stones.size(); ++j) {
            if (isGoodPair(stones[i], stones[j])) {
                ++result;
            }
        }
    }
    cout << result << endl;

    Stone& s0 = stones[0];
    Stone& s1 = stones[1];
    Stone& s2 = stones[2];
    tuple<int128_t, int128_t, int128_t> p1 = {s1.px - s0.px, s1.py - s0.py, s1.pz - s0.pz};
    tuple<int128_t, int128_t, int128_t> v1 = {s1.vx - s0.vx, s1.vy - s0.vy, s1.vz - s0.vz};
    tuple<int128_t, int128_t, int128_t> p2 = {s2.px - s0.px, s2.py - s0.py, s2.pz - s0.pz};
    tuple<int128_t, int128_t, int128_t> v2 = {s2.vx - s0.vx, s2.vy - s0.vy, s2.vz - s0.vz};

    auto p1xp2 = cross(p1, p2);
    auto v1xp2 = cross(v1, p2);
    auto p1xv2 = cross(p1, v2);

    int128_t t1 = -dot(p1xp2, v2) / dot(v1xp2, v2);
    int128_t t2 = -dot(p1xp2, v1) / dot(p1xv2, v1);

    tuple<int128_t, int128_t, int128_t> p1abs = {s1.px, s1.py, s1.pz};
    tuple<int128_t, int128_t, int128_t> v1abs = {s1.vx, s1.vy, s1.vz};
    tuple<int128_t, int128_t, int128_t> p2abs = {s2.px, s2.py, s2.pz};
    tuple<int128_t, int128_t, int128_t> v2abs = {s2.vx, s2.vy, s2.vz};

    auto c1 = add(p1abs, scalarMul(v1abs, t1));
    auto c2 = add(p2abs, scalarMul(v2abs, t2));

    auto v = scalarDiv(sub(c2, c1), t2 - t1);
    auto p = sub(c1, scalarMul(v, t1));

    auto [x, y, z] = p;
    int128_t result2 = x + y + z;
    cout << result2 << endl;
}