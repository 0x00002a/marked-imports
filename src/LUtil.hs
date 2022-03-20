module LUtil where


mconcatInfix :: (Monoid m) => m -> [m] -> m
mconcatInfix v [] = mempty
mconcatInfix v [x] = x
mconcatInfix v (x:xs) = x <> v <> mconcatInfix v xs
