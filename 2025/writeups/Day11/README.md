## Day 11

There comes a time in a Sheinxy's life where you look at the input before coding.

This time was today :)

## The Input

We have a graph represented by a list of nodes and their neighbours.

I just split by ':' to get the name of the node, and I split by ' ' to get the values.

```hs
type Input = M.Map String [String]

parseInput :: String -> Input
parseInput = M.fromList . map parseLine . lines
    where parseLine raw = let (key, ':' : rest) = break (== ':') raw in (key, words rest)
```

## Part One: I looked at the input

### The Problem

We want to find the number of paths between `you` and `out`.

### The Solution

When I read thay puzzle, it reminded me of [day 07](../Day07)'s part 2.

If we can only move forward in the graph (i.e, there is no cycle in this directed graph), then
we can compute this solution recursively:
- the number of paths from a to b is the sum of the number of paths from each neighbour of a to b.

So I used [neato](https://graphviz.org/docs/layouts/neato/) to display the graph (after manually changing
the input into a dot using a lot of regex), and I got this:

![The graph](./graph.png)

It is a [DAG](https://en.wikipedia.org/wiki/Directed_acyclic_graph), so no cycle :)

```hs
countPathFromTo :: String -> String -> Input -> Output
countPathFromTo from to input = memoFix go from
    where go f key | key == to    = 1
                   | key == "out" = 0
                   | otherwise = sum [f next | next <- input ! key]

partOne :: Input -> Output
partOne = countPathFromTo "you" "out"
```

countPathFromTo simply counts the number of path between two nodes.

If the "from" node is `out`, and it's not the target node, then there are no paths possible.

## Part Two: that was easy

### The Problem

We need to find all the paths from `svr` to `out` that go through both `fft` and `dac`.

### The Solution

This is a DAG :)

This means that there is only one way possible: either we can go from `fft` to `dac`,
or from `dac` to `fft`.

Once we know that, we just need to find the number of paths between `srv` and the first one,
then the number of paths between the second one and `out`. Multiply these two together with the number of paths between the first and the second, and you've got your solution!

```hs
partTwo :: Input -> Output
partTwo input | fftToDac /= 0 && dacToFft /= 0 = error "Something went wrong. Please check your input. (There can't be both a path from fft to dac and from dac to fft)"
              | fftToDac == 0 = countPathFromTo "svr" "dac" input * dacToFft * countPathFromTo "fft" "out" input
              | otherwise = countPathFromTo "svr" "fft" input * fftToDac * countPathFromTo "dac" "out" input
    where fftToDac = countPathFromTo "fft" "dac" input
          dacToFft = countPathFromTo "dac" "fft" input
```

## Conclusion

See you soon for the end <3
