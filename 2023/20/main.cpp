#include <bits/stdc++.h>

using namespace std;

class Module {
public:
    using Ptr = shared_ptr<Module>;

    Module(string name) : name(move(name)) {}

    virtual vector<tuple<Module::Ptr, Module*, bool>> signal(Module* sender, bool high) = 0;
    
    virtual void addChild(Module::Ptr child) {
        children.push_back(child);
    }

    virtual void addParent(const string& parent) {}

    const string& getName() const {
        return name;
    }

    vector<tuple<Module::Ptr, Module*, bool>> sendToChildren(bool high) {
        vector<tuple<Module::Ptr, Module*, bool>> result;
        for (auto& child : children) {
            result.emplace_back(child, this, high);
        }
        return result;
    }

    virtual ~Module() = default;

// protected:
    string name;
    vector<Module::Ptr> children;
};

class Broadcaster : public Module {
public:
    Broadcaster(string name) : Module(move(name)) {}
    vector<tuple<Module::Ptr, Module*, bool>> signal(Module* sender, bool high) override {
        return sendToChildren(high);
    }
};

class FlipFlop : public Module {
public:
    FlipFlop(string name) : Module(move(name)) {}
    vector<tuple<Module::Ptr, Module*, bool>> signal(Module* sender, bool high) override {
        if (high) {
            return {};
        }
        isOn = !isOn;
        return sendToChildren(isOn);
    }
private:
    bool isOn = false;
};

class Conjuction : public Module {
public:
    Conjuction(string name) : Module(move(name)) {}
    vector<tuple<Module::Ptr, Module*, bool>> signal(Module* sender, bool high) override {
        if (lastSignalFromParent[sender->getName()] != high) {
            if (high) ++countHighs;
            else --countHighs;

            lastSignalFromParent[sender->getName()] = high;
        }

        bool sendHigh = countHighs != lastSignalFromParent.size();
        return sendToChildren(sendHigh);
    };

    void addParent(const string& parent) override {
        lastSignalFromParent[parent] = false;
    }
private:
    int countHighs = 0;
    unordered_map<string, bool> lastSignalFromParent;
};

class Sink : public Module {
public:
    Sink(string name) : Module(move(name)) {}
    vector<tuple<Module::Ptr, Module*, bool>> signal(Module* sender, bool high) override {
        return {};
    }
};

pair<Module::Ptr, string> parseLine(const string& str) {
    regex pattern(R"((.*) ->(.*))");

    smatch matches;
    Module::Ptr module = nullptr;
    string childrenStr;
    if (regex_search(str, matches, pattern)) {
        string name = matches[1];
        childrenStr = matches[2];
        if (name == "broadcaster") {
            module = make_shared<Broadcaster>(name);
        } else if (name[0] == '%') {
            module = make_shared<FlipFlop>(name.substr(1));
        } else if (name[0] == '&') {
            module = make_shared<Conjuction>(name.substr(1));
        }
    }
    assert(module != nullptr);
    return {module, childrenStr};
}

vector<string> parseChildStr(const auto& str) {
    stringstream ss(str);
    string tmp;
    vector<string> result;
    while (getline(ss, tmp, ',')) {
        result.push_back(tmp.substr(1));
    }
    return result;
}

void toDot(const unordered_map<string, Module::Ptr>& modules) {
    cout << "digraph {\n";

    for (const auto& [name, module] : modules) {
        string label;
        if (dynamic_pointer_cast<Conjuction>(module)) {
            label = "Con";
        } else if (dynamic_pointer_cast<FlipFlop>(module)) {
            label = "FF";
        } else if (dynamic_pointer_cast<Broadcaster>(module)) {
            label = "BC";
        }
        for (const auto& next : module->children) {
            cout << name << " -> " << next->getName() << " [label=\"  " << label << " \"]\n";
        }
    }

    cout << "}" << endl;
}

int64_t lcmVec(const vector<int64_t>& nums) {
    int64_t result = 1;
    for (auto num : nums) {
        result = lcm(result, num);
    }
    return result;
}

int main() {
    unordered_map<string, Module::Ptr> modules;

    unordered_map<string, string> childStrMap;
    string tmp;
    while(getline(cin, tmp)) {
        auto [module, childStr] = parseLine(tmp);
        modules[module->getName()] = module;
        childStrMap[module->getName()] = childStr;
    }

    string rxParent;
    unordered_set<string> rxGrandParents;
    for (const auto& [name, childStr] : childStrMap) {
        auto childrenNames = parseChildStr(childStr);
        for (const auto& childName : childrenNames) {
            auto& childModule = modules[childName];
            if (childModule == nullptr) {
                childModule = make_shared<Sink>(childName);
            }
            modules[name]->addChild(childModule);
            childModule->addParent(name);
            if (childName == "rx") {
                rxParent = name;
            }
        }
    }

    for (const auto& [name, module] : modules) {
        bool hasRxParentAsChild = false;
        for (auto& child : module->children) {
            if (child->getName() == rxParent) {
                hasRxParentAsChild = true;
                break;
            }
        }
        if (hasRxParentAsChild) {
            rxGrandParents.insert(name);
        }
    }

    // toDot(modules);
    // return 0;

    const int buttonPushes = 1000;
    int64_t lows = 0;
    int64_t highs = 0;
    int64_t result = 0;
    int64_t result2 = 0;
    vector<int64_t> rxGrandParentHighs;
    for (int i = 0; i < buttonPushes || !rxGrandParents.empty(); ++i) {
        // cout << "Button press " << i << endl;
        queue<tuple<Module::Ptr, Module*, bool>> q;
        q.emplace(modules["broadcaster"], nullptr, false);
        while (!q.empty()) {
            auto [module, parent, high] = q.front();
            q.pop();
            
            if (parent != nullptr && rxGrandParents.contains(parent->getName()) && high) {
                rxGrandParentHighs.push_back(i + 1);
                rxGrandParents.erase(parent->getName());
            }

            if (high) ++highs;
            else ++lows;

            auto newSignals = module->signal(parent, high);
            for (auto& sig : newSignals) {
                q.emplace(move(sig));
            }
        }
        if (i == buttonPushes - 1) {
            result = lows * highs;
        }
    }
    cout << result << endl;

    result2 = lcmVec(rxGrandParentHighs);
    cout << result2 << endl;
}