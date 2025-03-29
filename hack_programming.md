**Title:** Secure Password Hashing: bcrypt vs. MD5

**Summary:**  While both bcrypt and MD5 are hashing algorithms, bcrypt is designed for password security, offering salt and adaptive cost factors to resist brute-force and rainbow table attacks, unlike MD5 which is computationally inexpensive and vulnerable.

**Good Code (Python with bcrypt):**

```python
import bcrypt

def hash_password(password):
    salt = bcrypt.gensalt()  # Generate a unique salt for each password
    hashed = bcrypt.hashpw(password.encode('utf-8'), salt)
    return hashed.decode('utf-8')

def check_password(password, hashed_password):
    return bcrypt.checkpw(password.encode('utf-8'), hashed_password.encode('utf-8'))

# Example usage
password = "mysecretpassword"
hashed = hash_password(password)
print(f"Hashed password: {hashed}")
print(f"Password matches: {check_password(password, hashed)}")

```

**Bad Code (Python with MD5):**

```python
import hashlib

def hash_password(password):
    hashed = hashlib.md5(password.encode('utf-8')).hexdigest()
    return hashed

def check_password(password, hashed_password):
    return hashlib.md5(password.encode('utf-8')).hexdigest() == hashed_password

#Example Usage (INSECURE!)
password = "mysecretpassword"
hashed = hash_password(password)
print(f"Hashed password: {hashed}")
print(f"Password matches: {check_password(password, hashed)}")

```


**Key Takeaways:**

* **Salt:** bcrypt uses a unique salt for each password, preventing pre-computed rainbow table attacks. MD5 lacks this crucial security feature.
* **Adaptive Cost Factor:** bcrypt's cost factor increases the computation time required for hashing, making brute-force attacks significantly slower. MD5 has a fixed computation time, making it easier to crack.
* **Security:** bcrypt is a modern, cryptographically secure algorithm specifically designed for password hashing. MD5 is outdated and has known vulnerabilities.  It is considered cryptographically broken for password hashing.
* **One-Way Function:**  Both are designed as one-way functions, but the strength and computational cost of reversing the hashing differ drastically.  MD5 is far easier to reverse than bcrypt.
* **Library Support:**  Using established libraries like `bcrypt` ensures you benefit from security patches and best practices.  Improper implementation of MD5 can introduce additional vulnerabilities.


**Note:**  Always use a well-vetted and regularly updated library for cryptographic functions.  Never implement your own cryptographic algorithms unless you are a seasoned cryptography expert.  The examples provided are for illustrative purposes only and should not be used in a production environment without thorough review and adaptation to your specific security needs.
