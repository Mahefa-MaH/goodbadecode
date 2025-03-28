# Good Code Example:  Calculates the area of a rectangle.
func calculate_rectangle_area(length, width):
	return length * width

# Bad Code Example:  Calculates the area of a rectangle with unnecessary complexity and inconsistent naming.
func get_rect_area(l, w):
	var area = l * w
	return area * 1 #Unnecessary multiplication

#Good code example: Uses a function to encapsulate logic for cleaner code.
func is_within_range(value, min, max):
	return value >= min and value <= max

#Bad Code Example: Duplicates logic instead of using a function.
func check_value(value):
	if value >= 10 and value <= 20:
		return true
	else:
		return false

#Good Code Example: Uses descriptive variable names.
func calculate_average(numbers):
	var sum = 0
	for number in numbers:
		sum += number
	return sum / numbers.size()

#Bad Code Example: Uses unclear variable names.
func avg(nums):
	var s = 0
	for n in nums:
		s += n
	return s / nums.size()

