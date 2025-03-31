**Title:** Secure vs. Insecure Password Handling in Python

**Summary:**  The good code uses bcrypt for secure password hashing, preventing vulnerabilities like rainbow table attacks, while the bad code uses insecure methods vulnerable to simple cracking.


**Good Code:**

```python
import bcrypt

def hash_password(password):
  """Hashes a password using bcrypt."""
  salt = bcrypt.gensalt()
  hashed = bcrypt.hashpw(password.encode('utf-8'), salt)
  return hashed.decode('utf-8')

def check_password(password, hashed_password):
  """Checks a password against a bcrypt hash."""
  return bcrypt.checkpw(password.encode('utf-8'), hashed_password.encode('utf-8'))


#Example Usage
password = "mysecretpassword"
hashed = hash_password(password)
print(f"Hashed password: {hashed}")
print(f"Password matches: {check_password(password, hashed)}")
print(f"Password does not match: {check_password('wrongpassword', hashed)}")

```


**Bad Code:**

```python
import hashlib

def hash_password(password):
  """Hashes a password using SHA-1 (INSECURE)."""
  return hashlib.sha1(password.encode('utf-8')).hexdigest()

def check_password(password, hashed_password):
  """Checks a password against a SHA-1 hash (INSECURE)."""
  return hashlib.sha1(password.encode('utf-8')).hexdigest() == hashed_password


#Example Usage (INSECURE)
password = "mysecretpassword"
hashed = hash_password(password)
print(f"Hashed password: {hashed}")
print(f"Password matches: {check_password(password, hashed)}")
print(f"Password does not match: {check_password('wrongpassword', hashed)}")

```


**Key Takeaways:**

* **Salt and pepper:** The good code uses bcrypt which inherently incorporates salting, making it resistant to rainbow table attacks.  The bad code uses SHA-1 without salting, making it vulnerable.  Even with salting, SHA-1 is now considered cryptographically weak.
* **Stronger Algorithm:** bcrypt is a key derivation function specifically designed for password hashing and is computationally expensive, making brute-force attacks much harder. SHA-1 is a general-purpose hashing algorithm and not designed for password security.  It's much faster to compute, making it easier to crack.
* **One-way function:** Both functions strive to be one-way, but bcrypt is far more robust in this regard.  SHA-1 is susceptible to various attacks that might allow for reversal (though very difficult).
* **Security Libraries:**  The good code leverages a dedicated security library (`bcrypt`), ensuring the use of well-vetted and regularly updated algorithms and practices. The bad code uses a general-purpose library (`hashlib`) for a task it's not ideally suited for.
* **Error Handling:**  While not explicitly shown, robust error handling should be added to both examples (e.g., handling exceptions during hashing or checking).  This is especially crucial in production code.


**Note:**  Never use the bad code in a production environment.  The security implications are significant.  Always prioritize using well-vetted and industry-standard cryptographic libraries for password handling.
