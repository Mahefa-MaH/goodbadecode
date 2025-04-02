namespace GoodCode {
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Intrinsic;

    operation GoodBellStatePreparation(qubits : Qubit[]) : Unit {
        H(qubits[0]);
        CNOT(qubits[0], qubits[1]);
    }

    operation GoodBellStateMeasurement(qubits : Qubit[]) : Result[] {
        return M(qubits);
    }
}


namespace BadCode {
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Intrinsic;

    operation BadBellStatePreparation(qubits : Qubit[]) : Unit {
        // Incorrect: Only applies Hadamard to the first qubit. Doesn't create Bell state.
        H(qubits[0]); 
    }

    operation BadBellStateMeasurement(qubits : Qubit[]) : Result[] {
        // Incorrect: Measures only the first qubit, ignoring the entanglement.
        return [M(qubits[0])];  
    }
}
