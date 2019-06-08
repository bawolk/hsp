module PipeFolds
    ( PipeInfo(..)
    , getPipeInfo
    )
where

import qualified Control.Foldl                 as Fold

import           Types
import           Parser

data PipeInfo = PipeInfo
        {
          haspp :: !Bool
        , hasdate :: !Bool
        , haspwd :: !Bool
        , hpToken :: !(Maybe Token)
        , envTokens :: ![Token]
        , shellTokens :: ![Token]
        , hasextra :: !Bool
        , globToken :: !(Maybe Token)
        }

hasppF :: Fold.Fold Token Bool
hasppF = Fold.any isPpType
  where
    isPpType t = case t of
        Pp    -> True
        Hpp _ -> True
        Spp   -> True
        Fpp   -> True
        _     -> False

hasdateF :: Fold.Fold Token Bool
hasdateF = Fold.any isDate
  where
    isDate (Identifier "date") = True
    isDate _                   = False

haspwdF :: Fold.Fold Token Bool
haspwdF = Fold.any isPwd
  where
    isPwd (Identifier "pwd") = True
    isPwd _                  = False

hpTokenF :: Fold.Fold Token (Maybe Token)
hpTokenF = Fold.find isHp
  where
    isHp (Hp _) = True
    isHp _      = False

collect :: (a -> Bool) -> Fold.Fold a [a]
collect pred = Fold.Fold step [] reverse
    where step x y = if pred y then y : x else x

envTokensF :: Fold.Fold Token [Token]
envTokensF = collect pred
  where
    pred (Env _) = True
    pred _       = False

shellTokensF :: Fold.Fold Token [Token]
shellTokensF = collect pred
  where
    pred (Shell _) = True
    pred _         = False

hasextraF :: Fold.Fold Token Bool
hasextraF = Fold.any isExtra
  where
    isExtra t = case t of
        Hp _ -> True
        Sp   -> True
        Fp   -> True
        _    -> False

globTokenF :: Fold.Fold Token (Maybe Token)
globTokenF = Fold.find isGlob
  where
    isGlob (Glob _) = True
    isGlob _        = False

getPipeInfo :: [Token] -> PipeInfo
getPipeInfo = Fold.fold
    (   PipeInfo
    <$> hasppF
    <*> hasdateF
    <*> haspwdF
    <*> hpTokenF
    <*> envTokensF
    <*> shellTokensF
    <*> hasextraF
    <*> globTokenF
    )
