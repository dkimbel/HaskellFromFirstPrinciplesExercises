import Debug.Trace (trace)

inc = (+1)

twice = inc . inc

howManyTimes =
  inc (trace "A - I got eval'd" (1 + 1))
    + twice (trace "B - I got eval'd" (1 + 1))

howManyTimes' =
  let onePlusOne =
        trace "C - I got eval'd" (1 + 1)
  in inc onePlusOne + twice onePlusOne
