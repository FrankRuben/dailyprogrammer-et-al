module H { // Helpers
  let round_int_of_float = f => int_of_float(floor(f +. 0.5))
}

module L { // logic
  type bracket = { incomeCap: int, taxRatePercent: int }

  let brackets = [
    { incomeCap:  10000,  taxRatePercent:  0 },
    { incomeCap:  30000,  taxRatePercent: 10 },
    { incomeCap: 100000,  taxRatePercent: 25 },
    // sentinel (instead of given info "hole-number income amount up to 100,000,000"):
    { incomeCap:     -1,  taxRatePercent: 40 },
  ]

  let taxPercent = (income) => {
    let rec loop = (restIncome, restBrackets, lastIncomeCap, tax) => {
      switch restBrackets {
      | [] => raise(Failure("Bad bracket definition"))
      | [hd, ..._] when hd.incomeCap < 0 =>
          tax + (income - lastIncomeCap) * hd.taxRatePercent
      | [hd, ...tail] => {
          if (income < hd.incomeCap) {
            tax + (income - lastIncomeCap) * hd.taxRatePercent
          } else {
            loop(restIncome - hd.incomeCap, tail, hd.incomeCap, tax + (hd.incomeCap - lastIncomeCap) * hd.taxRatePercent)
          }
        }
      }
    }
    loop(income, brackets, 0, 0)
  }

  let fromPercent = (percent) => float_of_int(percent) /. 100.0;
  let overallPercent = (income) => taxPercent(income) / income;

  let tax = (income) => H.round_int_of_float(fromPercent(taxPercent(income)));

  let computeIncomeForOverall = (overallPercent) => {
    let overall = fromPercent(overallPercent)
    // Below we use an algebraic solution to compute the income from the overall tax rate.
    // For incomes above tax bracket we use the following definitions:
    //   x0         : income for tax bracket 0 (up to 10,000), always 10,000
    //   o0         : tax rate for tax bracket 0, always 0
    //   x1, x2, x3 : income for tax bracket 1 (income above 10,000 and up to 30,000),
    //                tax bracket 2 (income above 30,000 and up to 100,000),
    //                tax bracket 3 (income above 100,000)
    //   o1, o2, o3 : tax rate for tax brackets 1, 2 and 3
    //   X          : total income
    //   O          : overall tax rate
    // we know:
    //   x0 * o0 + x1 * o1 + x2 * o2 + x3 * o3  == X * O
    // and:
    //   x0 + x1 + x2 + x3 == X
    // from using both we can derive:
    //   O x0 + (O - o1) x1 + (O - o2) x2 + (O - o3) x3 == 0
    //
    // We cannot use this formula for directly computing the income from the overall tax rate,
    // since we have 3 unknowns. But we can use it per bracket, when we know into which bracket
    // the given overall tax rate falls, so we first need to compute these boundaries.
    // Then we get for the first bracket (where we know x0, o1 and X == x0 + x1):
    //   x1 == O x0 / (o1 - O)
    // And for the second bracket (where we know x0, x1, o1, o2 and X == x0 + x1 + x2):
    //   x2 == (O x0 + (O - o1) x1) / (o2 - O)
    // And for the third bracket (where we know all variables except x3, X == x0 + x1 + x2 + x3):
    //   x3 == (O x0 + (O - o1) x1 + (O - o2) x2) / (o3 - O)
    // Note: We could also use the formula from above to compute the overall tax rate for the maximum
    //   income of each tax bracket, but this can nearly as easily been done using the tax-function.

    // Ideally the following should be done for a variable number of tax brackets (and using less
    // variables) - and I'll also implement that ... once I'm getting money for it.
    let maxIncomeBracket0 = List.nth(brackets, 0).incomeCap;
    let taxRateBracket1 = fromPercent(List.nth(brackets, 1).taxRatePercent);
    let maxIncomeBracket1 = List.nth(brackets, 1).incomeCap;
    let deltaIncomeBracket1 = maxIncomeBracket1 - maxIncomeBracket0
    let maxOverallRate1 = float_of_int(tax(maxIncomeBracket1)) /. float_of_int(maxIncomeBracket1);
    let maxIncomeBracket2 = List.nth(brackets, 2).incomeCap;
    let deltaIncomeBracket2 = maxIncomeBracket2 - maxIncomeBracket1
    let taxRateBracket2 = fromPercent(List.nth(brackets, 2).taxRatePercent);
    let maxOverallRate2 = float_of_int(tax(maxIncomeBracket2)) /. float_of_int(maxIncomeBracket2);
    let taxRateBracket3 = fromPercent(List.nth(brackets, 3).taxRatePercent);

    let computeIncomeBracket1 = (overall) => {
      // first bracket: x1 == O x0 / (o1 - O)
      overall *. float_of_int(maxIncomeBracket0)
        /. (taxRateBracket1 -. overall)
    }
    let computeIncomeBracket2 = (overall) => {
      // second bracket: x2 == (O x0 + (O - o1) x1) / (o2 - O)
      (overall *. float_of_int(maxIncomeBracket0)
        +. (overall  -. taxRateBracket1) *. float_of_int(deltaIncomeBracket1))
           /. (taxRateBracket2 -. overall)
    }
    let computeIncomeBracket3 = (overall) => {
      // third bracket: x3 == (O x0 + (O - o1) x1 + (O - o2) x2) / (o3 - O)
      (overall *. float_of_int(maxIncomeBracket0)
        +. (overall  -. taxRateBracket1) *. float_of_int(deltaIncomeBracket1)
        +. (overall  -. taxRateBracket2) *. float_of_int(deltaIncomeBracket2))
           /. (taxRateBracket3 -. overall)
    }

    if (overall <= maxOverallRate1) {
      let f = float_of_int(maxIncomeBracket0)
           +. computeIncomeBracket1(overall)
      H.round_int_of_float(f)
    } else if (overall <= maxOverallRate2) {
      let f = float_of_int(maxIncomeBracket0)
           +. float_of_int(deltaIncomeBracket1)
           +. computeIncomeBracket2(overall)
      H.round_int_of_float(f)
    } else {
      let f = float_of_int(maxIncomeBracket0)
           +. float_of_int(deltaIncomeBracket1)
           +. float_of_int(deltaIncomeBracket2)
           +. computeIncomeBracket3(overall)
      H.round_int_of_float(f)
    }
  }

  type overallResult =
    | Income(int)
    | NotFound(string);

  let incomeForOverall = (overallPercent) => {
    let lastBracket = List.nth(brackets, List.length(brackets) - 1);
    if (overallPercent == 0) {
      Income(List.nth(brackets, 0).incomeCap);
    } else if (overallPercent >= lastBracket.taxRatePercent) {
      NotFound("too high");
    } else {
      Income(computeIncomeForOverall(overallPercent))
    }
  }
}

module T { // testing
  let do_log_ok = true;
  let do_log_error = true;
  let do_fail_error = true;
  let epsilon = 1;

  let assertIntPrint = (act, exp) => {
    // From the challenge: You may get somewhat different answers because of rounding, but as long
    //   as it's close that's fine; so we support an epsilon to give us that leeway.
    let isOk = abs(act - exp) <= epsilon;
    switch (isOk, do_log_ok, do_log_error, do_fail_error) {
    | (true, true, _, _)      => Printf.printf("OK -- act: %d, exp: %d\n", act, exp)
    | (false, _, true, false) => Printf.eprintf("ERR -- act: %d, exp: %d\n", act, exp)
    | (false, _, true, true)  => failwith(Printf.sprintf("ERR -- act: %d, exp: %d\n", act, exp))
    | _ => ()
    }
  }

  let assertStringPrint = (act, exp) => {
    let isOk = String.compare(act, exp) == 0;
    switch (isOk, do_log_ok, do_log_error, do_fail_error) {
    | (true, true, _, _)      => Printf.printf("OK -- act: %s, exp: %s\n", act, exp)
    | (false, _, true, false) => Printf.eprintf("ERR -- act: %s, exp: %s\n", act, exp)
    | (false, _, true, true)  => failwith(Printf.sprintf("ERR -- act: %s, exp: %s\n", act, exp))
    | _ => ()
    }
  }

  let assertOverallPrint = (actResult, expResult) => {
    switch (actResult, expResult) {
    | (L.Income(act), L.Income(exp)) => assertIntPrint(act, exp)
    | (L.NotFound(actMsg), L.NotFound(expMsg)) => assertStringPrint(actMsg, expMsg)
    | (L.Income(_), _) => failwith(Printf.sprintf("ERR -- act: Income, exp: NotFound\n"))
    | (L.NotFound(_), _) => failwith(Printf.sprintf("ERR -- act: NotFound, exp: Income\n"))
    }
  }
}

let main = () => {
  T.assertIntPrint(L.tax(0), 0)
  T.assertIntPrint(L.tax(10000), 0)
  T.assertIntPrint(L.tax(10009), 0)
  T.assertIntPrint(L.tax(10010), 1)
  T.assertIntPrint(L.tax(12000), 200)
  T.assertIntPrint(L.tax(18000), 800)
  T.assertIntPrint(L.tax(56789), 8697)
  T.assertIntPrint(L.tax(1234567), 473326)

  T.assertIntPrint(L.tax(256250), 82000)
  T.assertIntPrint(L.overallPercent(256250), 32)

  T.assertOverallPrint(L.incomeForOverall(0), L.Income(10000))
  T.assertOverallPrint(L.incomeForOverall(6), L.Income(25000))
  T.assertOverallPrint(L.incomeForOverall(9), L.Income(34375))
  T.assertOverallPrint(L.incomeForOverall(32), L.Income(256250))
  T.assertOverallPrint(L.incomeForOverall(40), L.NotFound("too high"))
}

main()

// See here: https://old.reddit.com/r/dailyprogrammer/comments/cdieag/20190715_challenge_379_easy_progressive_taxation/
// Run like this: dune exec ./dp379_easy_taxation.exe
// with the following content of file dune: (executable (name dp379_easy_taxation))

/* output:

OK -- act: 0, exp: 0
OK -- act: 0, exp: 0
OK -- act: 1, exp: 0
OK -- act: 1, exp: 1
OK -- act: 200, exp: 200
OK -- act: 800, exp: 800
OK -- act: 8697, exp: 8697
OK -- act: 473327, exp: 473326
OK -- act: 82000, exp: 82000
OK -- act: 32, exp: 32
OK -- act: 10000, exp: 10000
OK -- act: 25000, exp: 25000
OK -- act: 34375, exp: 34375
OK -- act: 256250, exp: 256250
OK -- act: too high, exp: too high

*/
