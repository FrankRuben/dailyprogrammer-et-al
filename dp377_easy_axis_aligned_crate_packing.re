module T { // Testing
  let do_log_ok = true;
  let do_log_error = true;
  let do_fail_error = true;

  let assertIntPrint = (act, exp) => {
    let isOk = (act == exp);
    switch (isOk, do_log_ok, do_log_error, do_fail_error) {
    | (true, true, _, _)      => Printf.printf("OK -- act: %d, exp: %d\n", act, exp)
    | (false, _, true, false) => Printf.eprintf("ERR -- act: %d, exp: %d\n", act, exp)
    | (false, _, true, true)  => failwith(Printf.sprintf("ERR -- act: %d, exp: %d\n", act, exp))
    | _ => ()
    }
  }
}

module L { // Logic
  let fit1 = (crateX, crateY, boxX, boxY) => {
    // Two dimensions, can't rotate boxes.
    let nbX = crateX / boxX; let nbY = crateY / boxY;
    (nbX * nbY);
  }

  let fit2 = (crateX, crateY, boxX, boxY) => {
    // Two dimensions, option of rotating all boxes by 90 degrees. Can't rotate boxes independently.
    let nbXX = crateX / boxX; let nbYY = crateY / boxY;
    let nbXY = crateX / boxY; let nbYX = crateY / boxX;
    max((nbXX * nbYY), (nbXY * nbYX));
  }

  let fit3 = (crateX, crateY, crateZ, boxX, boxY, boxZ) => {
    // Three dimensions, option of rotating all boxes by 90 degrees. Can't rotate boxes independently.
    let nbXX = crateX / boxX; let nbYY = crateY / boxY; let nbZZ = crateZ / boxZ;
    let nbXY = crateX / boxY; let nbYZ = crateY / boxZ; let nbZX = crateZ / boxX;
    let nbXZ = crateX / boxZ; let nbYX = crateY / boxX; let nbZY = crateZ / boxY;
    let maxN = (l) => List.fold_left((a, b) => a > b ? a : b, 0, l);
    maxN([(nbXX * nbYY * nbZZ), (nbXY * nbYZ * nbZX), (nbXZ * nbYX * nbZY),
          (nbXX * nbYZ * nbZY), (nbXZ * nbYY * nbZX), (nbXY * nbYX * nbZZ)]);
  }

  let fitn = (crateSizes, boxSizes) => {
    // N crate and box dimensions, boxes rotated in any of N! orientations, but not independently.
    let nbCrates = List.length(crateSizes);
    assert(nbCrates == List.length(boxSizes));

    let rec recurse = (usedCrates, usedBoxes, crateSizes, boxSizes, nbSoFar) => {
      let nbUsedCrates = 1 + List.length(usedCrates);
      let rec loopCrates = (crateIdx, crateMax) => {
        if (crateIdx == nbCrates) {
          crateMax;
        } else if (List.mem(crateIdx, usedCrates)) {
          loopCrates(crateIdx + 1, crateMax);
        } else {
          let crateSize = Array.get(crateSizes, crateIdx);
          let nextUsedCrates = [crateIdx, ...usedCrates];
          let rec loopBoxes = (boxIdx, boxMax) => {
            if (boxIdx == nbCrates) {
              boxMax;
            } else if (List.mem(boxIdx, usedBoxes)) {
              loopBoxes(boxIdx + 1, boxMax);
            } else {
              let boxSize = Array.get(boxSizes, boxIdx);
              let nextNbSoFar = nbSoFar * (crateSize / boxSize);
              if (nbUsedCrates < nbCrates) {
                loopBoxes(boxIdx + 1, max(boxMax,
                  recurse(nextUsedCrates, [boxIdx, ...usedBoxes],
                  crateSizes, boxSizes, nextNbSoFar)));
              } else {
                loopBoxes(boxIdx + 1, max(boxMax, nextNbSoFar));
              }
            }
          }
          loopCrates(crateIdx + 1, max(crateMax, loopBoxes(0, 0)));
        }
      }
      loopCrates(0, 0);
    }
    recurse([], [], Array.of_list(crateSizes), Array.of_list(boxSizes), 1);
  }
}

let main = () => {
  T.assertIntPrint(L.fit1(25, 18, 6, 5), 12);
  T.assertIntPrint(L.fit1(10, 10, 1, 1), 100);
  T.assertIntPrint(L.fit1(12, 34, 5, 6), 10);
  T.assertIntPrint(L.fit1(12345, 678910, 1112, 1314), 5676);
  T.assertIntPrint(L.fit1(5, 100, 6, 1), 0);

  T.assertIntPrint(L.fit2(25, 18, 6, 5), 15);
  T.assertIntPrint(L.fit2(12, 34, 5, 6), 12);
  T.assertIntPrint(L.fit2(12345, 678910, 1112, 1314), 5676);
  T.assertIntPrint(L.fit2(5, 5, 3, 2), 2);
  T.assertIntPrint(L.fit2(5, 100, 6, 1), 80);
  T.assertIntPrint(L.fit2(5, 5, 6, 1), 0);

  T.assertIntPrint(L.fit3(10, 10, 10, 1, 1, 1), 1000);
  T.assertIntPrint(L.fit3(12, 34, 56, 7, 8, 9), 32);
  T.assertIntPrint(L.fit3(123, 456, 789, 10, 11, 12), 32604);
  T.assertIntPrint(L.fit3(1234567, 89101112, 13141516, 171819, 202122, 232425), 174648);

  T.assertIntPrint(L.fitn([25, 18], [6, 5]), 15);
  T.assertIntPrint(L.fitn([12, 34], [5, 6]), 12);
  T.assertIntPrint(L.fitn([12345, 678910], [1112, 1314]), 5676);
  T.assertIntPrint(L.fitn([5, 5], [3, 2]), 2);
  T.assertIntPrint(L.fitn([5, 100], [6, 1]), 80);
  T.assertIntPrint(L.fitn([5, 5], [6, 1]), 0);

  T.assertIntPrint(L.fitn([10, 10, 10], [1, 1, 1]), 1000);
  T.assertIntPrint(L.fitn([12, 34, 56], [7, 8, 9]), 32);
  T.assertIntPrint(L.fitn([123, 456, 789], [10, 11, 12]), 32604);
  T.assertIntPrint(L.fitn([1234567, 89101112, 13141516], [171819, 202122, 232425]), 174648);

  T.assertIntPrint(L.fitn([123, 456, 789, 1011, 1213, 1415], [16, 17, 18, 19, 20, 21]), 1883443968);
  // those above run correctly, but the one below did not finish even after ~24 hrs:
  // print_endline(string_of_int(L.fitn(
  //    [180598, 125683, 146932, 158296, 171997, 204683, 193694, 216231, 177673, 169317, 216456, 220003, 165939, 205613, 152779, 177216, 128838, 126894, 210076, 148407],
  //    [1984, 2122, 1760, 2059, 1278, 2017, 1443, 2223, 2169, 1502, 1274, 1740, 1740, 1768, 1295, 1916, 2249, 2036, 1886, 2010])));
  // => 4281855455197643306306491981973422080000
}

main()
