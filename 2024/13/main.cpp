#include <bits/stdc++.h>

using namespace std;

int64_t solve(int64_t ax, int64_t ay, int64_t bx, int64_t by, int64_t px, int64_t py) {
    static int64_t costA = 3;
    static int64_t costB = 1;
    int64_t bnum = py * ax - px * ay;
    int64_t bden = by * ax - bx * ay;
    if (bnum % bden != 0) return 0;

    int64_t b = bnum / bden;
    if (b < 0) return 0;

    int64_t anum = px - bx * b;
    int64_t aden = ax;
    if (anum % aden != 0) return 0;

    int64_t a = anum / aden;
    if (a < 0) return 0;

    int64_t cost = a*costA + b*costB;
    return cost;
}

int main() {
    string tmp;
    int64_t result = 0;
    int64_t result2 = 0;
    int64_t offset = 10000000000000;

    int64_t ax, ay, bx, by, px, py;
    while(getline(cin, tmp)) {
        if (tmp.empty()) continue;
        stringstream ssa(tmp);
        ssa.ignore(12);
        ssa >> ax;
        ssa.ignore(4);
        ssa >> ay;

        getline(cin, tmp);
        stringstream ssb(tmp);
        ssb.ignore(12);
        ssb >> bx;
        ssb.ignore(4);
        ssb >> by;

        getline(cin, tmp);
        stringstream ssp(tmp);
        ssp.ignore(9);
        ssp >> px;
        ssp.ignore(4);
        ssp >> py;

        result += solve(ax, ay, bx, by, px, py);
        result2 += solve(ax, ay, bx, by, px + offset, py + offset);
    }
    cout << result << endl;
    cout << result2 << endl;
}
