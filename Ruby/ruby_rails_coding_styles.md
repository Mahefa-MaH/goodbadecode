# Good Code Example: Using ActiveRecord efficiently

User.where(active: true).where("created_at > ?", 1.week.ago).find_each do |user|
  user.send_welcome_email
end


# Bad Code Example: N+1 Query Problem

users = User.where(active: true)
users.each do |user|
  puts user.orders.count # This will execute a separate query for each user.
end

# Better Code Example: Avoiding N+1

users = User.where(active: true).includes(:orders)
users.each do |user|
  puts user.orders.count
end


# Good Code Example: Using strong parameters

def user_params
  params.require(:user).permit(:name, :email, :password)
end

# Bad Code Example: Mass assignment vulnerability

def user_params
  params[:user] # Vulnerable to mass assignment attacks.
end


# Good Code Example: DRY principle

def calculate_total(items)
  items.sum(&:price)
end

def calculate_discount(items)
  calculate_total(items) * 0.1 # Reuse calculate_total
end

# Bad Code Example: Repeating the same logic

def calculate_total(items)
  items.sum(&:price)
end

def calculate_discount(items)
  items.sum(&:price) * 0.1 # Duplicated logic
end


# Good Code Example: Using appropriate data types

class User < ApplicationRecord
  validates :age, numericality: { only_integer: true, greater_than_or_equal_to: 0 }
end


# Bad Code Example: Using inappropriate data types

class User < ApplicationRecord
  # age can be a string, leading to errors
end


# Good Code Example: Concise and readable code

def greet(name)
  "Hello, #{name}!"
end

# Bad Code Example: Unnecessarily verbose code

def greet(name)
  message = "Hello, "
  message += name
  message += "!"
  return message
end
