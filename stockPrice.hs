import GHC.Exts
findBestDeal arr = 
    let pairs = zip[0..] arr
        sortedPairs = sortWith(\pair -> negate(snd pair)) pairs
     in maximum [findBestSingleDeal sortedPairs pair | pair <- sortedPairs]

findBestSingleDeal sortedPairs pair =
    --value for all with higer index
    let higher = [snd toCheck | toCheck <- sortedPairs, fst toCheck > fst pair]
    in case higher of 
        [] -> -1
        _ -> head higher - snd pair