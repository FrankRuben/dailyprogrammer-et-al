package main

import "fmt"

type digitMap map[byte]int
type result struct {
	n int
	s string
}
type resultChan chan result

func recurse(numDigits, digitPos, restDigits, sumDigits int, prefix string,
	countDigits digitMap, resultChan resultChan) {
	if restDigits == 0 {
		matches := true
		for i, c := range prefix {
			sCnt := int(c - '0')
			cCnt, found := countDigits[byte('0'+i)]
			if !found {
				cCnt = 0
			}
			if sCnt != cCnt {
				matches = false
				break
			}
		}
		if matches {
			res := result{numDigits, prefix}
			resultChan <- res
		}
		return
	}

	l := len(prefix)
	for d, dPos := byte('0'), 0; d < byte('0'+numDigits); d, dPos = d+1, dPos+1 {
		newSumDigits := sumDigits + dPos
		if newSumDigits > numDigits {
			continue // drop impossible numbers: total sum of digits too large
		}
		newCountDigits := make(digitMap, 10)
		found, skip := false, false
	DigitLoop:
		for k, v := range countDigits {
			if d == k {
				if dPos < l && prefix[dPos] == byte('0'+v) {
					skip = true
					break DigitLoop // drop impossible numbers: too many occurences of new digit
				}
				newCountDigits[k] = v + 1
				found = true
			} else {
				newCountDigits[k] = v
			}
		}
		if skip {
			continue
		}
		if !found {
			if restDigits < dPos {
				continue // drop impossible numbers: too few occurences of new digit
			}
			newCountDigits[d] = 1
		}
		s := prefix + string(d)
		recurse(numDigits, digitPos+1, restDigits-1, newSumDigits, s, newCountDigits, resultChan)
	}
}

func build(numDigits int, resultChan resultChan) {
	// Doesn't make sense to start w/ '0', since then we'd already have a 1 for the first/'0' digit
	for d, dPos := byte('1'), 1; d < byte('0'+numDigits); d, dPos = d+1, dPos+1 {
		m := make(digitMap, 10)
		m[d] = 1
		s := "" + string(d)
		recurse(numDigits, 1, numDigits-1, dPos, s, m, resultChan)
	}
}

func main() {
	const minDigits, maxDigits = 4, 15
	resultChan := make(resultChan, 100)
	for numDigits := minDigits; numDigits <= maxDigits; numDigits++ {
		build(numDigits, resultChan)
	}
	close(resultChan)

	lastDigit := minDigits
	for res := range resultChan {
		for missingDigit := lastDigit + 1; missingDigit < res.n; missingDigit++ {
			fmt.Printf("%d: No self-descriptive number found\n", missingDigit)
		}
		lastDigit = res.n
		fmt.Printf("%d %s\n", res.n, res.s)
	}
}

// See: https://www.reddit.com/r/dailyprogrammer/comments/41tdzy/20160120_challenge_250_intermediate/
//
// Output:
// For all number of digits from 4 to 15:
// $ go run dp250.go
// 4 1210
// 4 2020
// 5 21200
// 6: No self-descriptive number found
// 7 3211000
// 8 42101000
// 9 521001000
// 10 6210001000
// 11 72100001000
// 12 821000001000
// 13 9210000001000
// 14 :2100000001000
// 15 ;21000000001000

// Runtime for 15 digits
// $ time ./dp250
// 15 ;21000000001000
// real	0m3.641s
// user	0m3.731s
// sys	0m0.116s
