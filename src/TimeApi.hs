module TimeApi where

import Data.Time
import Data.Fixed (Pico) 

checkForGrowth :: UTCTime -> UTCTime -> Int -> Bool
checkForGrowth time1 time2 cooldownInSec = isReadyToGrow
    where
        deltaTime = abs(diffUTCTime time1 time2)

        cooldownInPico :: NominalDiffTime
        cooldownInPico = fromInteger (toInteger cooldownInSec)

        isReadyToGrow = deltaTime >= cooldownInPico