#include <bits/stdc++.h>

using namespace std;

int value(char c) {
    switch(c) {
        case 'A': return 14;
        case 'K': return 13;
        case 'Q': return 12;
        case 'J': return 11;
        case 'T': return 10;
        default: return c - '0';
    }
}

int valueHand(const vector<int>& hand) {
    vector<int> ns(6, 0);

    vector<int> counts(15, 0);

    for (int card : hand) {
        ++counts[card];
    }

    for (int count : counts) {
        ++ns[count];
    }

    if (ns[5] == 1) return 7;
    if (ns[4] == 1) return 6;
    if (ns[3] == 1 && ns[2] == 1) return 5;
    if (ns[3] == 1) return 4;
    if (ns[2] == 2) return 3;
    if (ns[2] == 1) return 2;
    return 1;    
}

pair<vector<int>, int> valueHand2(const vector<int>& hand) {
    vector<int> ns(6, 0);

    vector<int> counts(15, 0);

    int jokers = counts[11];

    for (int card : hand) {
        ++counts[card];
    }

    int maxCount = 0;
    int maxCard = 0;
    for (int i = counts.size() - 1; i >= 0; --i) {
        if (i == 11) continue;
        if (counts[i] > maxCount) {
            maxCount = counts[i];
            maxCard = i;
        }
    }

    auto handToValue = hand;
    for (auto& card : handToValue) {
        if (card == 11) {
            card = maxCard;
        }
    }

    int value = valueHand(handToValue);

    auto weakJHand = hand;
    for (auto& card : weakJHand) {
        if (card == 11) {
            card = 1;
        }
    }

    return {weakJHand, value};  
}

int main() {
    vector<tuple<vector<int>, int, int>> hands;
    vector<tuple<vector<int>, int, int>> hands2;
    string hand;
    int bid;
    while (cin >> hand >> bid) {
        vector<int> convHand;
        for (char c : hand) {
            convHand.emplace_back(value(c));
        }
        hands.emplace_back(convHand, valueHand(convHand), bid);

        auto [convHand2, value2] = valueHand2(convHand);
        hands2.emplace_back(convHand2, value2, bid);
    }
    auto comp = [](const auto& left, const auto& right){
        auto [hl, vl, bl] = left;
        auto [hr, vr, br] = right;
        if (vl != vr) {
            return vl < vr;
        }
        for (int i = 0; i < hl.size(); ++i) {
            if (hl[i] != hr[i]) {
                return hl[i] < hr[i];
            }
        }
        return false;
    };

    sort(hands.begin(), hands.end(), comp);
    int result = 0;
    for (int i = 0; i < hands.size(); ++i) {
        result += (i+1)*get<2>(hands[i]);
    }
    cout << result << endl;

    sort(hands2.begin(), hands2.end(), comp);
    int result2 = 0;
    for (int i = 0; i < hands2.size(); ++i) {
        result2 += (i+1)*get<2>(hands2[i]);
    }
    cout << result2 << endl;
}