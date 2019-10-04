let mazeString = {|
...X.
.....
.X.XX
..X.X
.....|}

type square =
  | Free
  | Cheese

let intOfSquare = fun
  | Cheese => 1
  | Free => 0

let squareOfChar = ch => (ch == 'X' ? Cheese : Free)

let mazeOfString = str => {
  let lines = Str.split(Str.regexp("[\n]"), str);
  let n = List.hd(lines) |> String.length;
  let maze = Array.make_matrix(n, n, Free);
  lines
  |> List.rev
  |> List.iteri((rowIdx, row)
       => String.iteri((colIdx, cell)
            => maze[rowIdx][colIdx] = squareOfChar(cell), row));
  maze
}

let solveMaze = maze => {
  let n = Array.length(maze);
  let maxCounter = Array.make_matrix(2, n, 0); // we only need to memorize the last two rows

  let maxMove = (rowIdx, colIdx, maxCounterIdx) => {
    let cellValue     = intOfSquare(maze[rowIdx][colIdx]);
    let topNbrValue   = (rowIdx == n - 1) ? 0 : maxCounter[(maxCounterIdx + 1) land 1][colIdx];
    let rightNbrValue = (colIdx == n - 1) ? 0 : maxCounter[maxCounterIdx][colIdx + 1];
    cellValue + (rightNbrValue > topNbrValue ? rightNbrValue : topNbrValue);
  }

  let rec rowLoop = (rowIdx) => {
    if (rowIdx < 0) maxCounter[0][0] // our maximum is now stored in the lower left corner
    else {
      let maxCounterIdx = (n - 1 - rowIdx) land 1;
      let rec colLoop = (colIdx) => {
        if (colIdx < 0) rowLoop(rowIdx - 1)
        else {
          maxCounter[maxCounterIdx][colIdx] = maxMove(rowIdx, colIdx, maxCounterIdx)
          colLoop(colIdx - 1)
        }
      }
      colLoop(n - 1)
    }
  }
  rowLoop(n - 1); // we start from top right corner and move backward through the maze
}

let main = () => {
  let maze = mazeOfString(mazeString);
  print_endline("Maximum amount of cheese: " ++ string_of_int(solveMaze(maze)));
}

main()

// See here: https://programmingpraxis.com/2019/05/14/
// Run as: dune exec ./dd20190514_rat_eats_cheese.exe
// Output: Maximum amount of cheese: 3
