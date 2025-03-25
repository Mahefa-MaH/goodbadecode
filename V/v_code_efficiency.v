fn good_code() {
	mut x := 0
	for i := 0; i < 10; i++ {
		x += i
	}
	println(x)
}

fn bad_code() {
	mut x := 0
	for i := 0; i < 10; i++ {
		x = x + i // less efficient than +=
	}
	println(x)

}

fn main() {
	good_code()
	bad_code()
}
