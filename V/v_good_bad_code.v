module main

fn good_code() {
	mut x := 0
	for i in 0 .. 10 {
		x += i
	}
	println(x)
}

fn bad_code() {
	mut x := 0
	for i := 0; i < 10; i++ {
		x += i
	}
	println(x)
}

fn main() {
	good_code()
	bad_code()
}
