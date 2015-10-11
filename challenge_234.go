package main

import (
	"fmt"
	"strings"
	"time"
)

type FangNumType uint64
type DigitCountType uint8
type AllDigitsCountType [10]DigitCountType

type numberAndDigits struct { // pre-computed info for each potential fang number n
	n FangNumType        // the fang/number created
	c AllDigitsCountType // count digits in fang: c[0] = i -> 0 occurs i-times in n
}

type vampireAndFangs struct { // correct vampire number and its fangs
	v    FangNumType   // the vampire/number created
	fSet []FangNumType // the fang/numbers building the vampire
}

func (v vampireAndFangs) String() string {
	parts := []string{}
	parts = append(parts, fmt.Sprintf("%d=", v.v))
	for i, f := range v.fSet {
		if i == 0 {
			parts = append(parts, fmt.Sprintf("%d", f))
		} else {
			parts = append(parts, fmt.Sprintf("%d", f))
		}
	}
	return strings.Join(parts, "")
}

func makeFangs(callerNum, mult FangNumType, currLen, maxFangLen int,
	callerCountDigits AllDigitsCountType) []numberAndDigits {
	var fangs []numberAndDigits
	for d := FangNumType(0); d <= 9; d++ {
		num := callerNum + mult*d
		numCountDigits := callerCountDigits
		numCountDigits[d]++
		if currLen == maxFangLen {
			if d > 0 {
				fangs = append(fangs, numberAndDigits{n: num, c: numCountDigits})
			}
		} else if currLen == 1 || d > 0 {
			fangs = append(fangs, makeFangs(num, mult*10, currLen+1, maxFangLen, numCountDigits)...)
		}
	}
	return fangs
}

func addNDigitCount(digitCount ...AllDigitsCountType) AllDigitsCountType {
	switch len(digitCount) {
	case 0:
		return AllDigitsCountType{0}
	case 1:
		return digitCount[0]
	default:
		allDigitCount := digitCount[0]
		for _, dc := range digitCount[1:] {
			for i, c := range dc {
				allDigitCount[i] += c
			}
		}
		return allDigitCount
	}
}

func bitsAndLen(v FangNumType, fangDigitCount AllDigitsCountType) (int, bool) {
	vLen := int(0)
	for v > 0 {
		d := v % 10
		if fangDigitCount[d] > 0 {
			fangDigitCount[d]--
		} else {
			return 0, false // v has more occurences of d as given in our fangs
		}
		v = v / 10
		vLen++
	}
	return vLen, true
}

func testNFangs(numFangs int, maxFangLen int, setOfFangs []numberAndDigits) (vampireAndFangs, bool) {
	v, cntLast := FangNumType(1), 0
	setOfFangNs := make([]FangNumType, numFangs)
	setOfFangCounts := make([]AllDigitsCountType, numFangs)
	for i, f := range setOfFangs {
		lastDigit := f.n % 10
		if lastDigit == 0 {
			cntLast++
		}
		if cntLast > 1 {
			continue // Pairs of trailing zeros are not allowed.
		}
		v = v * f.n
		setOfFangNs[i] = f.n
		setOfFangCounts[i] = f.c
	}
	fangDigitCount := addNDigitCount(setOfFangCounts...)
	vLen, countOk := bitsAndLen(v, fangDigitCount)
	if countOk && (vLen == numFangs*maxFangLen) {
		return vampireAndFangs{v: v, fSet: setOfFangNs}, true
	}
	return vampireAndFangs{}, false
}

func recurseNFangs(numFangs int, maxFangLen int, currSetOfFangs, restFangs []numberAndDigits) []vampireAndFangs {
	if len(currSetOfFangs) == numFangs {
		panic("Bad recursion")
	}
	var vampires []vampireAndFangs
	for i, f := range restFangs {
		newSetOfFangs := append(currSetOfFangs, f)
		if len(newSetOfFangs) == numFangs {
			if newVampireAndFangs, ok := testNFangs(numFangs, maxFangLen, newSetOfFangs); ok {
				vampires = append(vampires, newVampireAndFangs)
			}
		} else {
			vampires = append(vampires, recurseNFangs(numFangs, maxFangLen, newSetOfFangs, restFangs[i+1:])...)
		}
	}
	return vampires
}

func main() { // go build . && echo 4 2 | ./dr234
	start := time.Now()
	var maxVampireLen, maxFangLen int
	fmt.Scanf("%d %d", &maxVampireLen, &maxFangLen)
	fangs := makeFangs(0, 1, 1, maxFangLen, AllDigitsCountType{0})
	vampires := recurseNFangs(maxVampireLen/maxFangLen, maxFangLen, []numberAndDigits{}, fangs)
	fmt.Printf("Found %d vampires (elapsed: %v):\n", len(vampires), time.Since(start))
	for _, v := range vampires {
		fmt.Printf("%s\n", v)
	}
}
