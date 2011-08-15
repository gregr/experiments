module CFG where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Control.Monad

data Grammar tok st = Fail
                    | Final st
                    | Alt (Grammar tok st) (Grammar tok st)
                    | Conc st [(Grammar tok st)]
                    | One tok
                    | Ref (Int, [tok]) (Grammar tok st)
  deriving Eq

instance (Ord tok, Show tok, Show st) => Show (Grammar tok st) where
  show pat = show' S.empty pat
    where show' _ Fail = "Fail"
          show' _ (Final st) = "Final " ++ show st
          show' keys (Alt pat1 pat2) = show' keys pat1 ++ " | " ++ show' keys pat2
          show' keys (Conc st pats) = "{" ++ show st ++ ": " ++ foldr (\a b -> a ++ " " ++ b) "" (map (show' keys) pats) ++ "}"
          show' _ (One tok) = show tok
          show' keys (Ref key pat) = if S.member key keys then "<" ++ show key ++ ">" else show' (S.insert key keys) pat

alt Fail pat = pat
alt pat Fail = pat
alt pat1 pat2 = Alt pat1 pat2

conc st [] = Final st
conc _ (Fail:_) = Fail
conc st (Final stf:pats) = conc (mplus st stf) pats
conc st pats = Conc st pats

finalize pat = finalize' S.empty pat
  where finalize' _ Fail = Fail
        finalize' _ (Final st) = Final st
        finalize' _ (One _) = Fail
        finalize' keys (Alt pat1 pat2) = alt (finalize' keys pat1) (finalize' keys pat2)
        finalize' keys (Conc st pats) = conc st $ map (finalize' keys) pats
        finalize' keys (Ref key pat) = if S.member key keys then Fail else finalize' (S.insert key keys) pat

deriv tok pat = deriv' M.empty tok pat
  where deriv' _ _ Fail = Fail
        deriv' _ _ (Final _) = Fail
        deriv' _ tok (One tok') = if tok == tok' then Final (return tok) else Fail
        deriv' keys tok (Alt pat1 pat2) = alt (deriv'' pat1) (deriv'' pat2)
          where deriv'' = deriv' keys tok
        deriv' _ tok (Conc _ []) = error "how did this happen?"
        deriv' keys tok (Conc st (pat:pats)) =
            alt fc $ deriv' keys tok $ conc st (finalize pat : pats)
          where fc = conc st $ (deriv' keys tok pat) : pats
        deriv' keys tok (Ref key pat) = case M.lookup key keys of
          Just dref -> dref
          --Nothing -> Fail
          Nothing -> dref
            where dref = Ref key' $ deriv' keys' tok pat
                  keys' = M.insert key' dref keys
                  (keyn, toks) = key
                  key' = (keyn, tok:toks)

match pat [] = finalize pat
match pat (tok:toks) = match (deriv tok pat) toks

type TestGrammar = Grammar Char [Char]
tests = [
  deriv 'f' (One 'b') == (Fail :: TestGrammar),
  deriv 'f' (conc [] [(One 'f'), (One 'b')]) == Conc "f" [One 'b']]
--  deriv (alt (conc (One 'f') (One 'b')) (conc (One 'f') (rep (One 'z')))) 'f' == Alt (One 'b') (Rep (One 'z')),
--  match (conc (One 'f') (rep (One 'b'))) "fbbb" == True,
--  match (conc (One 'f') (rep (One 'b'))) "fbzbb" == False,
--  match (conc (One 'f') (rep (alt (One 'b') (One 'z')))) "fbzbb" == True]

lang = Ref (0, "") $ alt (Final []) $ conc [] [One 'r', lang]
lang2 = Ref (1, "") $ alt (Final []) $ conc [] [lang2, One 'r']
lang3 = Ref (2, "") $ alt lang3 (Final [])
lang4 :: TestGrammar
lang4 = Ref (3, "") lang4
lang5 = Ref (4, "") $ conc [] [Final [], lang5]
lang6 = Ref (5, "") $ conc [] [lang6, Final []]
langS = Ref (6, "") $ alt (conc [] [langS, One '+', langS]) $ One 'n'
