{-# LANGUAGE GADTs #-}

module EvenCount where

import Data.List (nub, subsequences)

type Δ a q = q -> a -> q

-- DFA minus Q
data Automaton a q where
  Automaton ::
    (Eq q) =>
    { autoAlphabet :: [a],
      autoInitial :: q,
      autoFinal :: [q],
      autoDelta :: Δ a q
    } ->
    Automaton a q

autoIsFinal :: Eq q => Automaton a q -> q -> Bool
autoIsFinal auto a = a `elem` autoFinal auto

autoRun :: Eq q => Automaton a q -> [a] -> q
autoRun auto = foldl (autoDelta auto) (autoInitial auto)

type B = Bool

-- xor = + mod 2
xor :: B -> B -> B
xor = (/=)

(-->) :: B -> B -> B
False --> _ = True
True --> p = p

-- _ f as = unzip $ map f as
-- λ f A. (π_1[A'], π_2[A'])
--        where A' = f[A] // up to η conversion
mapAndUnzip :: (a -> (b, c)) -> [a] -> ([b], [c])
mapAndUnzip f as = aux as ([], [])
  where
    aux [] (ls, rs) = (reverse ls, reverse rs)
    aux (a : as) (ls, rs) = let (l, r) = f a in (l : ls, r : rs)

-- δ' A ts () = \bigcup f[ts]
--  where f (_, q) = { g (_, q) a | a in A_A }
--                 = { (q' in F_A, q') | q' = δ q a, a in A_A }
-- δ' A = λ ts 1. \bigcup {{ (q' in F_A, q') | q' = δ q a, a in A_A } | (_, q) in ts}
fδ :: Eq q => Automaton a q -> Δ () [(B, q)]
fδ auto ts () = concatMap f ts
  where
    f t = map (g t) (autoAlphabet auto)
    g (b, q) a = (autoIsFinal auto q', q')
      where
        q' = autoDelta auto q a

foldAuto :: Eq q => Automaton a q -> Automaton () [(B, q)]
foldAuto auto =
  Automaton
    { autoAlphabet = [()],
      autoDelta = fδ auto,
      autoInitial = [(autoIsFinal auto i, i)],
      autoFinal =
        [ [(True, f) | f <- fs]
          | fs <- subsequences (autoFinal auto),
            even (length fs)
        ]
    }
  where
    i = autoInitial auto

isOddAuto :: Automaton Int Bool
isOddAuto =
  Automaton
    { autoAlphabet = [0, 1],
      autoDelta = \_ a -> a == 1,
      autoInitial = False,
      autoFinal = [True]
    }

-- second-to-last char equals 'a'
plEqAAuto :: Automaton Char String
plEqAAuto =
  Automaton
    { autoAlphabet = "ab",
      autoInitial = "bb",
      autoFinal = ["aa", "ab"],
      autoDelta = \(_ : a) b -> a <> [b]
    }

red :: Eq q => [(B, q)] -> B
red = not . foldl1 xor . map fst

ai :: (a -> b) -> a -> (b, a)
ai f a = (f a, a)