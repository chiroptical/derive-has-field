module Import.Types where

newtype X = X {yHello :: String}

newtype A = B {b :: String}

newtype C = C String

data D = D {d :: String} | E {e :: String}

newtype F = F {fHello :: String}
