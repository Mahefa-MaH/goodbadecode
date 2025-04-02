module main

import os

// Good Code: Uses built-in functions and avoids unnecessary allocations.
fn good_code(path string) !string {
	return os.read_file(path)
}

// Bad Code:  Uses less efficient methods, potential for errors not handled.
fn bad_code(path string) string {
	mut data := []byte{}
	file, err := os.open(path) or { return '' } //Error handling is poor.
	defer file.close()
	mut buf := [1024]byte{} // Inefficient buffer size, multiple reads
	for {
		n, err := file.read(buf[0..])
		if err != nil {
			break //Ignoring error is bad practice
		}
		data << buf[0..n]
	}
	return unsafe { string(data) } //Unsafe conversion
}

fn main() {
	good_path := 'good.txt'
	bad_path := 'bad.txt'
	os.write_file(good_path, 'Good')!
	os.write_file(bad_path, 'Bad')!
	println('Good code result: ${good_code(good_path) or { 'Error' }}')
	println('Bad code result: ${bad_code(bad_path)}')
	os.remove(good_path)!
	os.remove(bad_path)!
}
