module StatData exposing ( StatData
                         , initialStatData
                         , clearStatData
                         , addDataPoint
                         , removeDataPoint
                         , isEmpty
                         , calcSum
                         , calcSum2
                         , calcMean
                         , populationStDev
                         , sampleStDev
                         , numOfDataPoint
                         )

import Dict as D

type alias StatData = D.Dict Float Int

initialStatData : StatData
initialStatData = D.empty

clearStatData : StatData -> StatData
clearStatData _ = initialStatData

addDataPoint : Float -> Int -> StatData -> StatData
addDataPoint val count statData =
    let updFunc x = case x of
                     Nothing -> Just count
                     Just c  -> Just (c + count)
    in D.update val updFunc statData

removeDataPoint : Float -> Int -> StatData -> StatData
removeDataPoint val count statData =
    let updFunc x = case x of
                     Nothing -> Nothing
                     Just c  -> if count < c
                                  then Just (c + count)
                                  else Nothing
        statData1 = D.update val updFunc statData
    in statData1

isEmpty : StatData -> Bool
isEmpty statData = (D.size statData) == 0

calcSum : StatData -> Maybe Float
calcSum statData =
    Just (D.foldl (\ val count curSum -> curSum + (toFloat count) * val) 0 statData)

calcSum2 : StatData -> Maybe Float
calcSum2 statData =
    Just (D.foldl (\ val count curSum -> curSum + (toFloat count) * val * val) 0 statData)

calcMean : StatData -> Maybe Float
calcMean statData = 
    if isEmpty statData
        then Nothing 
        else case calcSum statData of
                 Nothing -> Nothing
                 Just v -> Just (v / (toFloat (numOfDataPoint statData)))
 
-- population standard deviation
populationStDev : StatData -> Maybe Float
populationStDev statData =
    case calcMean statData of
        Nothing   -> Nothing
        Just mean ->
            case calcSum2 statData of
                Nothing    -> Nothing
                Just sum2  ->
                    let c     = toFloat (numOfDataPoint statData)
                        mean2 = sum2 / c
                    in Just (sqrt (mean2 - mean * mean))

-- sample standard deviation
sampleStDev : StatData -> Maybe Float
sampleStDev statData =
  if numOfDataPoint statData < 2
   then Nothing
   else
    case calcMean statData of
        Nothing   -> Nothing
        Just mean ->
            let sum = D.foldl (\ v c s -> s + (toFloat c) * (v - mean) ^ 2 ) 0 statData
            in Just (sqrt (sum / (toFloat (numOfDataPoint statData) - 1)))
            

numOfDataPoint : StatData -> Int
numOfDataPoint statData = D.foldl (\ _ count curSum -> curSum + count) 0 statData
