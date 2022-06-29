module TimeApi where

import Data.Time 

checkForGrowth :: UTCTime -> UTCTime -> Int -> Bool
checkForGrowth time1 time2 cooldownInSec = isReadyToGrow
    where
        -- secInDay = fromInteger $ toInteger 60*60*24
        -- deltaTime = utctDayTime time1 + (fromInteger $ toInteger cooldownInSec)

        -- firstCond = deltaTime >= secInDay && (utctDayTime time2 + secInDay > deltaTime) && (utctDay time2 > utctDay time1)
        -- secondCond = deltaTime < secInDay && ((utctDayTime time2 > deltaTime && utctDay time2 >= utctDay time1) || (utctDayTime time2 < deltaTime && utctDay time2 > utctDay time1))

        deltaTime = toRational $ diffUTCTime time1 time2

        cooldownInRational = toRational cooldownInSec

        isReadyToGrow = deltaTime >= cooldownInRational