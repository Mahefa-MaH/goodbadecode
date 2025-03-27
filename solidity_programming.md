**Title:** Efficient and Secure Solidity ERC-20 Token Implementation

**Summary:**  The key difference lies in robust error handling and secure math operations.  Good code prevents common vulnerabilities like reentrancy and overflow/underflow, while bad code exposes these risks.


**Good Code:**

```solidity
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";

contract MyERC20Token is ERC20 {
    constructor(string memory name, string memory symbol) ERC20(name, symbol) {}

    function transfer(address recipient, uint256 amount) public virtual override returns (bool) {
        require(balanceOf(msg.sender) >= amount, "ERC20: transfer amount exceeds balance");
        _transfer(msg.sender, recipient, amount);
        return true;
    }

    function _transfer(address sender, address recipient, uint256 amount) internal virtual override {
        super._transfer(sender, recipient, amount);
        emit Transfer(sender, recipient, amount); // ensure event is emitted for every transfer
    }

     function mint(address to, uint256 amount) public onlyOwner {
        _mint(to, amount);
    }

    modifier onlyOwner() {
        require(msg.sender == owner(), "Ownable: caller is not the owner");
        _;
    }

     address public owner;

    constructor(string memory _name, string memory _symbol) ERC20(_name, _symbol) {
         owner = msg.sender;
    }


}
```

**Bad Code:**

```solidity
pragma solidity ^0.6.0;

contract BadERC20Token {
    string public name;
    string public symbol;
    uint256 public totalSupply;
    mapping(address => uint256) public balances;

    constructor(string memory _name, string memory _symbol) public {
        name = _name;
        symbol = _symbol;
    }

    function transfer(address _to, uint256 _value) public returns (bool success) {
        balances[msg.sender] -= _value;  //Potential underflow
        balances[_to] += _value;         //Potential overflow
        return true;
    }
}
```

**Key Takeaways:**

* **SafeMath:** The good code implicitly uses SafeMath (through OpenZeppelin's ERC20 library in Solidity >=0.8.0 which has built-in overflow/underflow protection) or would explicitly include a SafeMath library for versions prior to 0.8.0.  The bad code lacks this crucial protection, leading to potential vulnerabilities.
* **Error Handling:** The good code uses `require` statements to check for insufficient balances before transferring tokens, preventing unexpected behavior. The bad code lacks proper error handling.
* **OpenZeppelin Usage:** Leveraging OpenZeppelin's audited and well-tested ERC20 implementation significantly reduces the risk of introducing vulnerabilities.  It provides a solid foundation to build upon.
* **Event Emission:** Good code ensures that the `Transfer` event is always emitted, which is crucial for tracking token transfers and interacting with other dApps.
* **Ownership and Access Control:** The Good Code includes basic ownership and access control, limiting minting capabilities to the contract owner, whereas the bad code lacks this essential feature, making it vulnerable to unauthorized token creation.
* **Solidity Version:** Using a modern and well-supported Solidity compiler version (like ^0.8.0) helps to leverage built-in security improvements and avoid known vulnerabilities.  The bad code uses an older, potentially less secure version.


This comparison highlights the importance of using established libraries, robust error handling, and secure mathematical operations when developing smart contracts in Solidity.  Failing to address these points can lead to severe security flaws and financial losses.
