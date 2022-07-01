module TimeApi where

import Data.Time
import Data.Fixed (Pico) 

checkForGrowth :: UTCTime -> UTCTime -> Int -> Bool
checkForGrowth time1 time2 cooldownInSec = isReadyToGrow
    where
        -- secInDay = fromInteger $ toInteger 60*60*24
        -- deltaTime = utctDayTime time1 + (fromInteger $ toInteger cooldownInSec)

        -- firstCond = deltaTime >= secInDay && (utctDayTime time2 + secInDay > deltaTime) && (utctDay time2 > utctDay time1)
        -- secondCond = deltaTime < secInDay && ((utctDayTime time2 > deltaTime && utctDay time2 >= utctDay time1) || (utctDayTime time2 < deltaTime && utctDay time2 > utctDay time1))

        deltaTime = abs(diffUTCTime time1 time2)

        cooldownInPico :: NominalDiffTime
        cooldownInPico = fromInteger (toInteger cooldownInSec)

        isReadyToGrow = deltaTime >= cooldownInPico