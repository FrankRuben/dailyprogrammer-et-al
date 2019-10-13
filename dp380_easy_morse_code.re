module T { // Testing
  let do_log_ok = true;
  let do_log_error = true;
  let do_fail_error = true;

  let assertIntPrint = (name, act, exp) => {
    let isOk = act == exp;
    switch (isOk, do_log_ok, do_log_error, do_fail_error) {
    | (true, true, _, _)      =>
      Printf.printf("OK: %s -- act: %d, exp: %d\n", name, act, exp)
    | (false, _, true, false) =>
      Printf.eprintf("ERR: %s -- act: %d, exp: %d\n", name, act, exp)
    | (false, _, true, true)  =>
      failwith(Printf.sprintf("ERR: %s -- act: %d, exp: %d\n", name, act, exp))
    | _ => ()
    }
  }

  let assertStringPrint = (name, act, exp) => {
    let isOk = String.compare(act, exp) == 0;
    switch (isOk, do_log_ok, do_log_error, do_fail_error) {
    | (true, true, _, _)      =>
      Printf.printf("OK: %s -- act: %s, exp: %s\n", name, act, exp)
    | (false, _, true, false) =>
      Printf.eprintf("ERR: %s -- act: %s, exp: %s\n", name, act, exp)
    | (false, _, true, true)  =>
      failwith(Printf.sprintf("ERR: %s -- act: %s, exp: %s\n", name, act, exp))
    | _ => ()
    }
  }

  let assertOkPrint = (name, isOk, msg) => {
    switch (isOk, do_log_ok, do_log_error, do_fail_error) {
    | (true, true, _, _)      =>
      Printf.printf("OK: %s -- %s\n", name, msg)
    | (false, _, true, false) =>
      Printf.eprintf("ERR: %s -- %s\n", name, msg)
    | (false, _, true, true)  =>
      failwith(Printf.sprintf("ERR: %s -- %s", name, msg))
    | _ => ()
    }
  }
}

module H { // Helpers
  let countChar = (str, ch) => {
    let n = String.length(str);
    let rec loop (i, count) = {
      if (i == n) {
        count
      } else {
        loop(i + 1, count + (String.get(str, i) == ch ? 1 : 0))
      }
    }
    loop(0, 0)
  }

  let countCharInRow = (str, ch) => {
    let n = String.length(str);
    let rec loop (i, currCount, maxCount) = {
      if (i == n) {
        maxCount
      } else if (String.get(str, i) == ch) {
        loop(i + 1, currCount + 1, maxCount)
      } else {
        loop(i + 1, 0, max(currCount, maxCount))
      }
    }
    loop(0, 0, 0)
  }

  let isPerfectlyBalanced = (str, ch1, ch2) => {
    assert(ch1 !== ch2);
    countChar(str, ch1) == countChar(str, ch2);
  }

  let isPalindrome = (str) => {
    let n = String.length(str);
    let rec loop (i) = {
      if (i == n) {
        true
      } else if (String.get(str, i) == String.get(str, n - i - 1)) {
        loop(i + 1);
      } else {
        false
      }
    }
    loop(0)
  }
}

module type MorseStringType = {
  type t = pri string;        // type t ('private type') is now incompatible with string
  let ofString: string => t;  // so we need functions to map to and from string
  let toString: t => string;
  let length: t => int;
  let asString: string => t;  // and one to make an already morsed string a `t`
  let ofBitwiseInt: int => t;
  let subToBitwiseInt: (t, int, int) => int;
  let toBitwiseInt: t => int; // int is large enough for what we want here: 2^13

  let countDash: t => int;    // and for convenience we provide wrappers around our string helpers
  let countDot: t => int;
  let countDashInRow: t => int;
  let countDotInRow: t => int;
  let isPerfectlyBalanced: t => bool;
  let isPalindrome: t => bool;
}

module MorseString: MorseStringType {
  type t = string;

  let morseOfLetter = (letter) => {
    switch (Char.uppercase_ascii(letter)) {
    | 'A' =>  ".-"
    | 'B' =>  "-..."
    | 'C' =>  "-.-."
    | 'D' =>  "-.."
    | 'E' =>  "."
    | 'F' =>  "..-."
    | 'G' =>  "--."
    | 'H' =>  "...."
    | 'I' =>  ".."
    | 'J' =>  ".---"
    | 'K' =>  "-.-"
    | 'L' =>  ".-.."
    | 'M' =>  "--"
    | 'N' =>  "-."
    | 'O' =>  "---"
    | 'P' =>  ".--."
    | 'Q' =>  "--.-"
    | 'R' =>  ".-."
    | 'S' =>  "..."
    | 'T' =>  "-"
    | 'U' =>  "..-"
    | 'V' =>  "...-"
    | 'W' =>  ".--"
    | 'X' =>  "-..-"
    | 'Y' =>  "-.--"
    | 'Z' =>  "--.."
    | _ => raise(Failure(Printf.sprintf("Bad char %c", letter)))
    }
  }

  let morseOfString = (str) => {
    let n = String.length(str);
    let buf = Buffer.create(String.length(str));
    let rec loop = (i) => {
      if (i == n) {
        Buffer.contents(buf);
      } else {
        Buffer.add_string(buf, morseOfLetter(String.get(str, i)));
        loop(i + 1);
      }
    }
    loop(0);
  }

  let ofString = s => morseOfString(s);
  let toString = s => s;
  let length = s => String.length(s);

  let asString = s => {
    String.iter(ch => assert(ch == '.' || ch == '-'), s);
    s;
  }

  let ofBitwiseInt = i => {
    let buf = Buffer.create(64);
    let rec loop = (num) => {
      if (num == 0) {
        Buffer.add_char(buf, '.');
        Buffer.contents(buf);
      } else if (num == 1) {
        Buffer.add_char(buf, '-');
        Buffer.contents(buf);
      } else {
        Buffer.add_char(buf, (num land 1) == 1 ? '-' : '.');
        loop(num lsr 1);
      }
    }
    loop(i);
  }

  let subToBitwiseInt = (s, startIncl, n) => {
    assert(startIncl >= 0);
    let endExcl = startIncl + n;
    assert(endExcl <= String.length(s));
    let rec loop = (i, num, powerOf2) => {
      if (i >= endExcl) {
        num
      } else if (String.get(s, i) == '.') {
        loop(i + 1, num, powerOf2 * 2);
      } else {
        loop(i + 1, powerOf2 + num, powerOf2 * 2);
      }
    }
    loop(startIncl, 0, 1);
  }

  let toBitwiseInt = s => {
    subToBitwiseInt(s, 0, String.length(s));
  }

  let countDash = ms => H.countChar(toString(ms), '-');
  let countDot = ms => H.countChar(toString(ms), '.');
  let countDashInRow = ms => H.countCharInRow(toString(ms), '-');
  let countDotInRow = ms => H.countCharInRow(toString(ms), '.');
  let isPerfectlyBalanced = ms => H.isPerfectlyBalanced(toString(ms), '-', '.');
  let isPalindrome = ms => H.isPalindrome(toString(ms));
}

module L { // Logic
  // We wrap the required application logic so that we can loop over an input file just once,
  // running all processors in that single pass, accumulating results over all passes and run all
  // tests in the end, once the complete file has been read. Not that the API used here is not
  // generic enough that we won't have to close over additional state for some processors.

  type accumulatorType = {str: string, num: int};

  type resultType =
  | Ok(string)
  | OkMessage(string, string)
  | ErrorInts(string, int, int)
  | ErrorMessage(string, string);

  // process a single line, both its original string value plus the already converted morse string:
  type lineHandler = (string, MorseString.t) => accumulatorType;
  // aggregate the result from the `lineHandler` with the result of file-processing up to now:
  type fileAccumulator = (accumulatorType, accumulatorType) => accumulatorType;
  // evaluate final result over all lines:
  type doneHandler = accumulatorType => resultType;

  type lineWiseHandler = {
    lineHandler: lineHandler,
    fileAccumulator: fileAccumulator,
    doneHandler: doneHandler
  }

  let makeLineWiseAccums = handlers => Array.make(List.length(handlers), {str: "", num: 0}) |> Array.to_list;

  let applyLineWiseHandlers = (line, morseString, handlers, accList) => {
    List.map2((handler, acc) => {
      let curr = handler.lineHandler(line, morseString);
      let next = handler.fileAccumulator(acc, curr);
      next;
    }, handlers, accList);
  }

  let applyDoneHandlers = (handlers, accList) => {
    List.iter2((handler, acc) => {
      switch (handler.doneHandler(acc)) {
      | Ok(name)                  =>
        T.assertOkPrint(name, true, Printf.sprintf("%d/'%s'", acc.num, acc.str))
      | OkMessage(name, msg)      =>
        T.assertOkPrint(name, true, Printf.sprintf("%s: %d/'%s'", msg, acc.num, acc.str))
      | ErrorInts(name, act, exp) =>
        T.assertIntPrint(name, act, exp)
      | ErrorMessage(name, msg)   =>
        T.assertOkPrint(name, false, msg)
      }
    }, handlers, accList);
  }
}

let main = () => {
  let smorse = (str) => {
    open MorseString;
    toString(ofString(str));
  }
  T.assertStringPrint("sos", smorse("sos"), "...---...");
  T.assertStringPrint("daily", smorse("daily"), "-...-...-..-.--");
  T.assertStringPrint("programmer", smorse("programmer"), ".--..-.-----..-..-----..-.");
  T.assertStringPrint("bits", smorse("bits"), "-.....-...");
  T.assertStringPrint("three", smorse("three"), "-.....-...");

  {
    open MorseString;
    T.assertOkPrint("autotomous",
      // autotomous encodes to '.-..--------------..-...', which has 14 dashes in a row:
      countDashInRow(ofString("autotomous")) == 14, "14 dashes in a row");
    T.assertOkPrint("counterdemonstrations",
      // "counterdemonstrations" is one of two 21-letter words that's perfectly balanced:
      isPerfectlyBalanced(ofString("counterdemonstrations")), "balanced");
    T.assertOkPrint("protectorate",
      // "protectorate" is 12 letters long and [...] a palindrome
      isPalindrome(ofString("protectorate")), "palindrome");
  }

  let expectMatch = (name, act, exp) => {
    act == exp ? L.Ok(name) : L.ErrorInts(name, act, exp)
  }

  let expectSingle = (name, count) =>
    switch count {
    | 0 => L.ErrorMessage(name, "None found")
    | 1 => L.Ok(name)
    | _ => L.ErrorMessage(name, "Found too many")
    }

  let lineWiseHandlers: list(L.lineWiseHandler) = [
    { // It contains 172,823 words.
      lineHandler: (_line, _ms) => {str: "", num: 1},
      fileAccumulator: (acc, curr) => {str: acc.str, num: acc.num + curr.num},
      doneHandler: (acc) => expectMatch("count-words", acc.num, 172823)
    },
    { // If you encode them all, you would get a total of 2,499,157 dots ...
      lineHandler: (_line, ms) => {str: "", num: MorseString.countDot(ms)},
      fileAccumulator: (acc, curr) => {str: acc.str, num: acc.num + curr.num},
      doneHandler: (acc) => expectMatch("count-dots", acc.num, 2499157)
    },
    { // ... and 1,565,081 dashes.
      lineHandler: (_line, ms) => {str: "", num: MorseString.countDash(ms)},
      fileAccumulator: (acc, curr) => {str: acc.str, num: acc.num + curr.num},
      doneHandler: (acc) => expectMatch("count-dashes", acc.num, 1565081)
    },
    { // Find the only sequence that's the code for 13 different words.
      module MorseStringMap = Map.Make({
        type t = MorseString.t;
        let compare = (ms1, ms2) => String.compare(MorseString.toString(ms1), MorseString.toString(ms2));
      });
      let countMap = ref(MorseStringMap.empty);
      {
        lineHandler: (_line, ms) => {
          countMap := switch (MorseStringMap.find(ms, countMap^)) {
          | knownCount => MorseStringMap.add(ms, knownCount + 1, countMap^)
          | exception Not_found => MorseStringMap.add(ms, 1, countMap^)
          };
          {str: "", num: 1}
        },
        fileAccumulator: (acc, curr) => {
          if (curr.num > 0) { str: curr.str, num: acc.num + curr.num }
          else acc
        },
        doneHandler: (_acc) => {
          let matches = MorseStringMap.filter((_ms, count) => count == 13, countMap^) |> MorseStringMap.bindings;
          switch (List.length(matches)) {
          | 0 => L.ErrorMessage("13-words", "None found")
          | 1 => {
              let (ms, count) = List.hd(matches);
              L.OkMessage("13-words", Printf.sprintf("%s, %d", MorseString.toString(ms), count))
            }
          | _ => L.ErrorMessage("13-words", "Found too many")
          }
        }
      }
    },
    { // Find the only word that has 15 dashes in a row.
      lineHandler: (line, ms) => {
        if (MorseString.countDashInRow(ms) == 15) { str: line, num: 1 }
        else { str: "", num: 0 }
      },
      fileAccumulator: (acc, curr) => {
        if (curr.num > 0) { str: curr.str, num: acc.num + curr.num }
        else acc
      },
      doneHandler: (acc) => expectSingle("15-dashes", acc.num)
    },
    { // Find the other 21-letter word that's perfectly balanced next to "counterdemonstrations"
      lineHandler: (line, ms) => {
        if (String.length(line) != 21) { str: "", num: 0 }
        else if (String.compare(line, "counterdemonstrations") == 0) { str: "", num: 0 }
        else if (MorseString.isPerfectlyBalanced(ms)) { str: line, num: 1 }
        else { str: "", num: 0 }
      },
      fileAccumulator: (acc, curr) => {
        if (curr.num > 0) { str: curr.str, num: acc.num + curr.num }
        else acc
      },
      doneHandler: (acc) => expectSingle("21-balanced", acc.num)
    },
    { // Find the only 13-letter word that encodes to a palindrome.
      lineHandler: (line, ms) => {
        if (String.length(line) != 13) { str: "", num: 0 }
        else if (MorseString.isPalindrome(ms)) { str: line, num: 1 }
        else { str: "", num: 0 }
      },
      fileAccumulator: (acc, curr) => {
        if (curr.num > 0) { str: curr.str, num: acc.num + curr.num }
        else acc
      },
      doneHandler: (acc) => expectSingle("13-palindrome", acc.num)
    },
    { // Find the other four 13-character sequences that do not appear *in* the encoding of any word.
      let usedBitwiseInts = Array.make(1 lsl 13, false); // 2^13 -> 8192, so an easy fit for an array
      let markSeqUsed = (ms, start) =>
        Array.set(usedBitwiseInts, MorseString.subToBitwiseInt(ms, start, 13), true);
      { lineHandler: (_line, ms) => {
          let n = MorseString.length(ms);
          if (n < 13) ()
          else Array.init(n - 12, i => i) |> Array.iter(i => markSeqUsed(ms, i));
          { str: "", num: 0 }
        },
        fileAccumulator: (acc, _curr) => { str: acc.str, num: acc.num },
        doneHandler: (_acc) => {
          let knownBitwiseInt = MorseString.toBitwiseInt(MorseString.asString("--.---.---.--"));
          let rec loop = (idxAsBitwiseInt, acc) => {
            if (idxAsBitwiseInt == Array.length(usedBitwiseInts)) acc
            else {
              if (Array.get(usedBitwiseInts, idxAsBitwiseInt)) loop(idxAsBitwiseInt + 1, acc)
              else if (idxAsBitwiseInt == knownBitwiseInt) loop(idxAsBitwiseInt + 1, acc)
              else loop(idxAsBitwiseInt + 1,
                        [MorseString.toString(MorseString.ofBitwiseInt(idxAsBitwiseInt)), ...acc])
            }
          }
          let unknownMs = loop(0, []);
          let nbUnknownMs = List.length(unknownMs);
          switch nbUnknownMs {
          | 4 => L.OkMessage("13-words", Printf.sprintf("%s, %d", String.concat("; ", unknownMs), nbUnknownMs))
          | _ => L.ErrorMessage("13-words", "Found unexpected")
          }
        }
      }
    },
  ];

  let loopInFile = (~maxLines = max_int, ic) => {
    let rec loop = (lineNbr, ic, accList) => {
      if (lineNbr < maxLines) {
        switch (input_line(ic)) {
        | exception End_of_file => {
            close_in(ic);
            L.applyDoneHandlers(lineWiseHandlers, accList);
          }
        | line => {
            let ms = MorseString.ofString(line);
            loop(lineNbr + 1, ic, L.applyLineWiseHandlers(line, ms, lineWiseHandlers, accList))
          }
        }
      } else {
        Printf.printf("Stopped early: %d - %d\n", lineNbr, maxLines);
        L.applyDoneHandlers(lineWiseHandlers, accList);
      }
    }
    loop(0, ic, L.makeLineWiseAccums(lineWiseHandlers))
  }

  switch (open_in("enable1.txt")) {
  | exception (Sys_error(msg)) => print_endline(msg)
  | ic => loopInFile(~maxLines = max_int, ic);
  }
}

main()

// See here: https://old.reddit.com/r/dailyprogrammer/comments/cmd1hb/20190805_challenge_380_easy_smooshed_morse_code_1/
// Run like this: OCAMLRUNPARAM=b OCAML_COLOR=never dune exec ./dp380_easy_morse_code.bc
// with the following content of file dune: (executables (names dp380_easy_morse_code))

/* output:

OK: sos -- act: ...---..., exp: ...---...
OK: daily -- act: -...-...-..-.--, exp: -...-...-..-.--
OK: programmer -- act: .--..-.-----..-..-----..-., exp: .--..-.-----..-..-----..-.
OK: bits -- act: -.....-..., exp: -.....-...
OK: three -- act: -.....-..., exp: -.....-...
OK: autotomous -- 14 dashes in a row
OK: counterdemonstrations -- balanced
OK: protectorate -- palindrome
OK: count-words -- 172823/''
OK: count-dots -- 2499157/''
OK: count-dashes -- 1565081/''
OK: 13-words -- -....--...., 13: 172823/''
OK: 15-dashes -- 1/'bottommost'
OK: 21-balanced -- 1/'overcommercialization'
OK: 13-palindrome -- 1/'intransigence'
OK: 13-words -- --.---.------; ---.---.-----; ---.----.----; ---.---.---.-, 4: 0/''

*/
