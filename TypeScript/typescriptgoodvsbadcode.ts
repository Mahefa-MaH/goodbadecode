// Good Code Example:  Using interfaces and type safety
interface User {
  id: number;
  name: string;
  email: string;
}

function greetUser(user: User): string {
  return `Hello, ${user.name}! Your ID is ${user.id}`;
}

const user: User = { id: 1, name: "John Doe", email: "john.doe@example.com" };
console.log(greetUser(user));


// Bad Code Example: Lack of type safety and unclear logic
let userId = 1;
let userName = "Jane Doe";
let userEmail;

function greet(id, name, email) {
  if (id && name) {
    console.log("Hello, " + name + "! Your ID is " + id);
  } else {
    console.log("Invalid user data");
  }
  return "some string"
}

greet(userId, userName);
