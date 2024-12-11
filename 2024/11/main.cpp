#include <bits/stdc++.h>

using namespace std;

vector<int64_t> split(int64_t stone) {
    if (stone == 0) {
        return {1};
    }

    auto stoneStr = to_string(stone);
    if (stoneStr.size() % 2 == 0) {
        return {stol(stoneStr.substr(0, stoneStr.size() / 2)), stol(stoneStr.substr(stoneStr.size() / 2, stoneStr.size() / 2))};
    }
        
    return {stone * 2024};
}

int64_t fillDp(vector<unordered_map<int64_t, int64_t>>& dp, int64_t stone, int64_t stepsLeft) {
    if (stepsLeft == 0) {
        return 1;
    }

    if (dp[stepsLeft].contains(stone)) {
        return dp[stepsLeft][stone];
    }

    vector<int64_t> splitStone = split(stone);
    int64_t result = 0;
    for (auto subStone : splitStone) {
        result += fillDp(dp, subStone, stepsLeft - 1);
    }
    dp[stepsLeft][stone] = result;
    return result;
}

int main() {
    vector<int64_t> stones;
    int64_t tmp;
    while (cin >> tmp) {
        stones.push_back(tmp);
    }

    int64_t maxSteps = 25;
    int64_t maxSteps2 = 75;
    vector<unordered_map<int64_t, int64_t>> dp(maxSteps2 + 1);

    int64_t result = 0;
    int64_t result2 = 0;
    for (auto stone : stones) { 
        result += fillDp(dp, stone, maxSteps);
        result2 += fillDp(dp, stone, maxSteps2);
    }
    cout << result << endl;
    cout << result2 << endl;
}