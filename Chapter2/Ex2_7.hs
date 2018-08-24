module Ex2_7 where

import ADT_Records

decreasePrice :: TimeMachine -> Float -> TimeMachine
decreasePrice tm percent = 
    let newPrice = (price tm) * (1 - percent / 100)
    in
    tm { price = newPrice }

decreasePrices :: [TimeMachine] -> Float -> [TimeMachine]
decreasePrices [] percent = []
decreasePrices (x:xs) percent = (decreasePrice x percent) : (decreasePrices xs percent)

