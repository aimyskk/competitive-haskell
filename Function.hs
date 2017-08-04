module Function (
  untilFix
) where

untilFix :: Eq t => (t -> t) -> t -> t
untilFix f x = let x1 = f x in if x1 == x then x else untilFix f x1
