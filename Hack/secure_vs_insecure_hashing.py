# Good Code Example:  Illustrates a more secure and maintainable approach.

import hashlib

def secure_hash(password):
    salt = "some_random_salt"  # In real applications, use a cryptographically secure random salt.
    salted_password = salt + password
    hashed_password = hashlib.sha256(salted_password.encode()).hexdigest()
    return hashed_password

#Example usage
password = "mysecretpassword"
hashed_password = secure_hash(password)
print(hashed_password)


# Bad Code Example:  Vulnerable to various attacks due to lack of salting and weak hashing.

def insecure_hash(password):
    return hashlib.md5(password.encode()).hexdigest()

# Example usage
password = "mysecretpassword"
insecure_hashed_password = insecure_hash(password)
print(insecure_hashed_password)

# Hack attempt - not recommended.  This is only for illustration purposes to show the weakness of insecure_hash.  NEVER do this in real-world applications.

# Example dictionary attack
import hashlib
passwords = ["password", "1234", "qwerty"]
for p in passwords:
    if insecure_hash(p) == insecure_hashed_password:
        print(f"Password cracked: {p}")
        break

