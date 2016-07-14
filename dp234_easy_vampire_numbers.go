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
			parts = append(parts, fmt.Sprintf("*%d", f))
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

/*
See: https://www.reddit.com/r/dailyprogrammer/comments/3moxid/20150928_challenge_234_easy_vampire_numbers/cvvz9nk

Result for 8 2:
Found 33 vampires (elapsed: 1.7984766s):
13578750=30*71*75*85
15977840=40*71*97*58
19896840=40*81*69*89
11934650=50*61*91*43
13659840=60*51*93*48
17278560=60*71*52*78
16248960=60*91*62*48
13677440=70*71*43*64
16584750=70*81*45*65
13427960=70*91*62*34
28139940=90*41*82*93
13789620=90*71*83*26
15744960=90*71*44*56
19459440=90*91*44*54
36837990=90*63*73*89
13485992=31*52*94*89
12284748=41*42*82*87
12487944=41*42*74*98
23958116=61*91*52*83
21588632=61*52*82*83
16576872=61*52*67*78
14837396=61*73*34*98
14367276=71*42*73*66
15787347=71*83*47*57
12798744=71*74*84*29
19813248=81*91*32*84
27184896=81*92*76*48
12988836=81*83*28*69
17348499=81*93*47*49
28299348=42*82*83*99
25788672=72*82*56*78
26489376=72*93*46*86
48773928=82*73*84*97
*/
