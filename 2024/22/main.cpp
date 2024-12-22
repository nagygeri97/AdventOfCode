#include <bits/stdc++.h>

using namespace std;

int64_t prune(int64_t secret) {
    return secret % 16777216;
}

int64_t mix(int64_t secret, int64_t mixer) {
    return secret ^ mixer;
}

int64_t next(int64_t secret) {
    secret = prune(mix(secret, secret * 64));
    secret = prune(mix(secret, secret / 32));
    secret = prune(mix(secret, secret * 2048));
    return secret;
}

int main() {
    int64_t secret;
    int64_t result = 0;
    map<deque<int>, int> counts;
    while (cin >> secret) {
        set<deque<int>> visited;
        deque<int> changes;
        int prevPrice = secret % 10;
        for (int i = 0; i < 2000; ++i) {
            secret = next(secret);
            int price = secret % 10;
            int diff = price - prevPrice;
            changes.push_back(diff);
            if (changes.size() > 4) {
                changes.pop_front();
                if (!visited.contains(changes)) {
                    visited.insert(changes);
                    counts[changes] += price;
                }
            }
            prevPrice = price;
        }
        result += secret;
    }
    cout << result << endl;

    int result2 = 0;
    for (const auto& [_, val] : counts) {
        result2 = max(result2, val);
    }
    cout << result2 << endl;
}