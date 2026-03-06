package main

import "fmt"

func isPrime(n int64) bool {
	if n < 2 {
		return false
	}
	if n == 2 {
		return true
	}
	if n%2 == 0 {
		return false
	}

	for d := int64(3); d*d <= n; d += 2 {
		if n%d == 0 {
			return false
		}
	}
	return true
}

func main() {
	const limit int64 = 200000
	var count int64

	for n := int64(2); n <= limit; n++ {
		if isPrime(n) {
			count++
		}
	}

	fmt.Println(count)
}
