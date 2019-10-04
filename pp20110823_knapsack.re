type objectToPack = { name: char, weight: int, value: int };
type packedObjects = { totalValue: int, names: list(char) };

let makeRandomObjects = (n, maxWeight, maxValue) => {
  Random.init(0);
  let aCode = Char.code('a');
  let objects = Array.init(n, i => {
    name:   Char.chr(aCode + i),
    weight: 1 + Random.int(maxWeight),
    value:  1 + Random.int(maxValue)
  });
  Array.sort((o1, o2) => compare(o1.weight, o2.weight), objects);
  objects;
}

let makeObjects = (n, maxWeight) => {
  // sample data from https://programmingpraxis.com/2011/08/23/knapsack/
  assert(n == 8); assert(maxWeight == 15);
  let objects = List.map(
     ((ni,  wi, vi)) => { name: ni, weight: wi, value: vi },
    [ ('a', 10, 21),
      ('b', 10, 31),
      ('c',  1, 13),
      ('d',  6,  9),
      ('e', 15,  2),
      ('f',  2, 10),
      ('g',  4, 24),
      ('h',  4, 15) ]) |> Array.of_list;
  Array.sort((o1, o2) => compare(o1.weight, o2.weight), objects);
  objects;
}

let stringOfNames = revNames => {
  let arr = Array.of_list(List.rev(revNames));
  String.init(Array.length(arr), i => Array.get(arr, i));
}

let knapsack = (objects, maxCapacity) => {
  let n = Array.length(objects);
  let maxValues = Array.make_matrix(n + 1, maxCapacity + 1, { totalValue: 0, names: [] });

  Array.init(n, i => i + 1) |> Array.iter(nbObjectsSoFar => { // nbObjectsSoFar is 1-based, but...
    let objectToPack = objects[nbObjectsSoFar - 1];           // ... the objects array is 0-based
    Array.init(maxCapacity, w => w + 1) |> Array.iter(capacitySoFar => { // skip the V[0,w] = 0
      let predMaxPackedObjects = maxValues[nbObjectsSoFar - 1][capacitySoFar];
      let predMaxValue = predMaxPackedObjects.totalValue;
      if (objectToPack.weight > capacitySoFar) {
        maxValues[nbObjectsSoFar][capacitySoFar] = predMaxPackedObjects;
      } else {
        let predFreeMaxPackedObjects = maxValues[nbObjectsSoFar - 1][capacitySoFar - objectToPack.weight];
        let predFreeMaxValue = predFreeMaxPackedObjects.totalValue;
        let newMaxValue = objectToPack.value + predFreeMaxValue;
        if (newMaxValue > predMaxValue) {
          maxValues[nbObjectsSoFar][capacitySoFar] = {
            totalValue: newMaxValue,
            names: [objectToPack.name, ...predFreeMaxPackedObjects.names]
          };
        } else {
          maxValues[nbObjectsSoFar][capacitySoFar] = predMaxPackedObjects;
        }
      }
    });
  });
  maxValues[n-1][maxCapacity];
}

let main = () => {
  let objects = makeObjects(8, 15);
  let res = knapsack(objects, 20);
  Printf.printf("Max: %d [%s]", res.totalValue, stringOfNames(res.names));
}

main();

// See here: https://programmingpraxis.com/2011/08/23/knapsack/
// Run as: dune exec ./pp20110823_knapsack_as_posted.exe
// Output: Max: 83 [chgb]
