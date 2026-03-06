package main

import "fmt"

func main() {
	const iterations int64 = 50000000
	const modVal int64 = 2147483647

	var x int64 = 1
	var acc int64 = 0

	for i := int64(0); i < iterations; i++ {
		x = (x*1664525 + 1013904223) % modVal
		acc += x % 1024
	}

	fmt.Println(acc)
}
