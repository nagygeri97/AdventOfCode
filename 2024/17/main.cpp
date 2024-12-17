#include <bits/stdc++.h>

using namespace std;

struct State {
    int64_t A;
    int64_t B;
    int64_t C;
    int64_t IP;
    vector<int64_t> output;
};

int64_t readRegisterInputLine() {
    int64_t result;
    string tmp;
    getline(cin, tmp);
    stringstream ss(tmp);
    ss.ignore("Register _: "s.size());
    ss >> result;
    return result;
}

vector<int64_t> readProgramInputLine() {
    string tmp;
    vector<int64_t> result;
    getline(cin, tmp);
    getline(cin, tmp);
    stringstream ss(tmp);
    ss.ignore("Program: "s.size());
    while(getline(ss, tmp, ',')) {
        result.emplace_back(stoll(tmp));
    }
    return result;
}

int64_t resolveComboOperand(int64_t operand, State& state){
    switch(operand) {
        case 0:
        case 1:
        case 2:
        case 3: return operand;
        case 4: return state.A;
        case 5: return state.B;
        case 6: return state.C;
    }
    throw runtime_error("Invalid combo operand: "s + to_string(operand));
}

void adv(int64_t operand, State& state) {
    state.A = state.A / (1 << resolveComboOperand(operand, state));
}

void bxl(int64_t operand, State& state) {
    state.B ^= operand;
}

void bst(int64_t operand, State& state) {
    state.B = resolveComboOperand(operand, state) % 8;
}

void jnz(int64_t operand, State& state) {
    if (state.A != 0) {
        // -2 so the subsequent IP increment brings it back to operand.
        state.IP = operand - 2;
    } 
}

void bxc(int64_t operand, State& state) {
    state.B ^= state.C;
}

void out(int64_t operand, State& state) {
    state.output.emplace_back(resolveComboOperand(operand, state) % 8);
}

void bdv(int64_t operand, State& state) {
    state.B = state.A / (1 << resolveComboOperand(operand, state));
}

void cdv(int64_t operand, State& state) {
    state.C = state.A / (1 << resolveComboOperand(operand, state));
}

void runOpcode(int64_t opcode, int64_t operand, State& state) {
    switch(opcode) {
        case 0: adv(operand, state); break;
        case 1: bxl(operand, state); break;
        case 2: bst(operand, state); break;
        case 3: jnz(operand, state); break;
        case 4: bxc(operand, state); break;
        case 5: out(operand, state); break;
        case 6: bdv(operand, state); break;
        case 7: cdv(operand, state); break;
        default: throw runtime_error("Invalid opcode: " + to_string(opcode));
    }
}

void printOutput(State& state) {
    for (int i = 0; i < state.output.size(); ++i) {
        cout << (i == 0 ? ""s : ","s) <<  state.output[i];
    }
    cout << endl;
}

void runProgram(const vector<int64_t>& program, State& state) {
    while (state.IP < program.size()) {
        runOpcode(program[state.IP], program[state.IP + 1], state);
        state.IP += 2;
    }
}

void printComboOperand(int64_t operand) {
    switch(operand) {
        case 0:
        case 1:
        case 2:
        case 3: cout << operand; break;
        case 4: cout << "A"; break;
        case 5: cout << "B"; break;
        case 6: cout << "C"; break;
        default: throw runtime_error("Invalid combo operand: "s + to_string(operand));
    }
}

void printInstruction(int64_t opcode, int64_t operand) {
    switch(opcode) {
        case 0: cout << "adv "; printComboOperand(operand); break;
        case 1: cout << "bxl " << operand; break;
        case 2: cout << "bst "; printComboOperand(operand); break;
        case 3: cout << "jnz " << operand; break;
        case 4: cout << "bxc -"; break;
        case 5: cout << "out "; printComboOperand(operand); break;
        case 6: cout << "bdv "; printComboOperand(operand); break;
        case 7: cout << "cdv "; printComboOperand(operand); break;
        default: throw runtime_error("Invalid opcode: " + to_string(opcode));
    }
}

int64_t oneStep(int64_t A) {
    int64_t B = A % 8;
    B ^= 1;
    int64_t C = A >> B;
    B ^= 5;
    B ^= C;
    return B % 8;
}

optional<int64_t> bt(int64_t A, int64_t i, const vector<int64_t>& program) {
    if (i < 0) return A;
    vector<int64_t> candidateJs;
    A <<= 3;
    for (int64_t j = 0; j < 8; ++j) {
        int64_t candidateA = A + j;
        if (oneStep(candidateA) == program[i]) {
            candidateJs.emplace_back(j);
        }
    }

    for (auto j : candidateJs) {
        auto result = bt(A+j, i-1, program);
        if (result.has_value()) {
            return result;
        }
    }
    return nullopt;
}

int main() {
    State state = {readRegisterInputLine(), readRegisterInputLine(), readRegisterInputLine(), 0, {}};
    vector<int64_t> program = readProgramInputLine();
    runProgram(program, state);
    printOutput(state);

    auto result2 = bt(0, program.size() - 1, program);
    cout << *result2 << endl;

}