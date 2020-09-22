module Tokens.Tech where


universe :: (Enum a, Bounded a) => [a]
universe = [minBound .. maxBound]
