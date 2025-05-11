**Title:**  Smalltalk: Efficient vs. Inefficient Message Passing

**Summary:**  Efficient Smalltalk leverages optimized message sends and avoids unnecessary object creation, while inefficient code suffers from redundant computations and excessive object instantiation, impacting performance.

**Good Code:**

```smalltalk
Transcript show: 'Starting Calculation'.
n := 10000.
sum := 0.

1 to: n do: [:i | 
  sum := sum + i.
].

Transcript show: 'Sum: ', sum printString; cr.
Transcript show: 'Calculation Complete'.
```

This example directly calculates the sum within a single loop.  It minimizes object creation and utilizes Smalltalk's efficient message-sending mechanism.  The `printString` message is only sent once at the end for efficient output.


**Bad Code:**

```smalltalk
Transcript show: 'Starting Calculation'.
n := 10000.
sum := 0.

1 to: n do: [:i | 
  sum := sum + (Number with: i).  "Inefficient object creation"
].

Transcript show: 'Sum: ', (sum printString) , '!' ; cr.   "Redundant object creation"
Transcript show: 'Calculation Complete'.
```

This code inefficiently creates a new `Number` object for every iteration within the loop, increasing memory allocation and garbage collection overhead. Additionally, it creates an unnecessary string object by sending `printString` within the `Transcript show:` message.


**Key Takeaways:**

* **Minimize Object Creation:** Avoid unnecessary object instantiation within loops or frequently called methods.  Directly operate on existing objects when possible.
* **Efficient Message Passing:**  Understand the cost of message sends. Sending messages to already created and cached objects is faster.
* **Avoid Redundant Operations:** Combine operations to minimize the number of messages sent.  This reduces computational overhead.
* **String Concatenation:**  Concatenate strings efficiently.  Using `printString` only once is generally more efficient than multiple concatenations.
* **Understand your Framework:**  Smalltalk frameworks offer optimizations, use them.  This example demonstrates direct use of the `Transcript` for output, utilizing framework efficiency.
