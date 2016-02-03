--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module P where

--------------------------------------------------------------------------------

import Control.Applicative

import Pretty
import Token
import Lexer
import AST

import Prelude hiding (union)
import Data.Set (Set, union, (\\))
import qualified Data.Set as S

--------------------------------------------------------------------------------

newtype P a = MkP { runParser :: AlexState -> Either String a }

--------------------------------------------------------------------------------

instance Functor P where
    fmap f (MkP m) = MkP $ fmap f . m

instance Applicative P where
    pure x = MkP (\s -> Right x)

    (MkP f) <*> (MkP x) = MkP $ \s -> case f s of
        Left err -> Left err
        Right f' -> case x s of
            Left err -> Left err
            Right x' -> Right (f' x')

instance Monad P where
    (MkP m) >>= f = MkP $ \s -> case m s of
        Left err -> Left err
        Right r  -> let (MkP m') = f r in m' s

    return = pure

--------------------------------------------------------------------------------

mkParseState :: String -> AlexState
mkParseState xs = AlexState {
    alex_pos = AlexPn 0 1 1,
    alex_inp = xs,
    alex_chr = '\n',
    alex_bytes = [],
    alex_scd = 0
}

parseFile :: FilePath -> P a -> IO (Either String a)
parseFile fp p = do
    xs <- readFile fp
    return $ runParser p (mkParseState xs)

-- | `parseError tkn' raises a parser error as a result of encountering `tkn'.
parseError :: TokenP -> P a
parseError (tkn, pos) = MkP $ \s -> Left $ render $
    pp pos <+> text "Parse error: unexpected" <+> pp tkn

--------------------------------------------------------------------------------

toPosn :: AlexPosn -> Posn
toPosn (AlexPn a l c) = FilePosn l c

mkBind :: TokenP -> LambdaForm -> P Bind
mkBind (TVar var, pos) lf = return $ MkBind var lf (toPosn pos)

mkLambdaForm :: [Var] -> UpdateFlag -> [Var] -> Expr -> P LambdaForm
mkLambdaForm fvs uf vs expr = return $ MkLambdaForm fvs uf vs expr

mkLetE :: AlexPosn -> [Bind] -> Expr -> P Expr
mkLetE pos bs expr = return $ LetE bs expr (toPosn pos)

mkLetRecE :: AlexPosn -> [Bind] -> Expr -> P Expr
mkLetRecE pos bs expr = return $ LetRecE bs expr (toPosn pos)

mkCaseE :: AlexPosn -> Expr -> Alts -> P Expr
mkCaseE pos expr alts = return $ CaseE expr alts (toPosn pos)

mkAppE :: TokenP -> [Atom] -> P Expr
mkAppE (TVar var, pos) as = return $ AppE var as (toPosn pos)

mkCtrE :: TokenP -> [Atom] -> P Expr
mkCtrE (TCtr ctr, pos) as = return $ CtrE ctr as (toPosn pos)

mkOpE :: TokenP -> [Atom] -> P Expr
mkOpE (TPrimOp op, pos) as = return $ OpE op as (toPosn pos)

mkLitE :: TokenP -> P Expr
mkLitE (TPrimInt val, pos) = return $ LitE val (toPosn pos)

mkAlgAlt :: TokenP -> [Var] -> Expr -> P AlgAlt
mkAlgAlt (TCtr ctr, pos) vs expr = return $ AAlt ctr vs expr (toPosn pos)

mkPrimAlt :: TokenP -> Expr -> P PrimAlt
mkPrimAlt (TPrimInt val, pos) expr = return $ PAlt val expr (toPosn pos)

mkDefaultVar :: TokenP -> Expr -> P DefaultAlt
mkDefaultVar (TVar var, pos) expr = return $ DefaultVar var expr (toPosn pos)

mkDefault :: AlexPosn -> Expr -> P DefaultAlt
mkDefault pos expr = return $ Default expr (toPosn pos)

mkVar :: TokenP -> P Atom
mkVar (TVar var, pos) = return $ VarAtom var (toPosn pos)

mkInt :: TokenP -> P Atom
mkInt (TPrimInt val, pos) = return $ LitAtom val (toPosn pos)

type GetFVS a = Set Var -> a -> Set Var

exprFVS :: GetFVS Expr
exprFVS bvs (LetE binds expr _) =
  let bvs' = S.fromList (map bindName binds) `union` bvs in
  S.unions (bindFVS bvs <$> binds) `union` exprFVS bvs' expr
exprFVS bvs (LetRecE binds expr _) =
  let bvs' = S.fromList (map bindName binds) `union` bvs in
  S.unions (bindFVS bvs' <$> binds) `union` exprFVS bvs' expr
exprFVS bvs (CaseE expr alts _) = exprFVS bvs expr `union` altsFVS bvs alts
exprFVS bvs (AppE f atoms _) =
  (if f `elem` bvs then id else S.insert f) (S.unions (atomFVS bvs <$> atoms))
exprFVS bvs (CtrE c atoms _) = S.unions (atomFVS bvs <$> atoms)
exprFVS bvs (OpE _ atoms _) = S.unions (atomFVS bvs <$> atoms)
exprFVS bvs (LitE _ _) = S.empty

bindFVS :: GetFVS Bind
bindFVS bvs (MkBind bv lf _) = lambdaFormFVS (S.insert bv bvs) lf

lambdaFormFVS :: GetFVS LambdaForm
lambdaFormFVS bvs (MkLambdaForm fvs _ vs expr) =
  exprFVS (S.fromList vs `union` bvs) expr

atomFVS :: GetFVS Atom
atomFVS bvs (VarAtom v _)
  | v `elem` bvs = S.empty
  | otherwise = S.singleton v
atomFVS bvs (LitAtom _ _) = S.empty

altsFVS :: GetFVS Alts
altsFVS bvs (AlgAlts alts def) =
  defaultAltFVS bvs def `union` S.unions (algAltFVS bvs <$> alts)
altsFVS bvs (PrimAlts alts def) =
  defaultAltFVS bvs def `union` S.unions (primAltFVS bvs <$> alts)

algAltFVS :: GetFVS AlgAlt
algAltFVS bvs (AAlt _ vs expr _) = exprFVS (S.fromList vs `union` bvs) expr

primAltFVS :: GetFVS PrimAlt
primAltFVS bvs (PAlt _ expr _) = exprFVS bvs expr

defaultAltFVS :: GetFVS DefaultAlt
defaultAltFVS bvs (Default expr _) = exprFVS bvs expr
defaultAltFVS bvs (DefaultVar v expr _) = exprFVS (S.insert v bvs) expr

validateFVS :: Prog -> LambdaForm -> (Set Var, Set Var)
validateFVS (MkProg binds) (MkLambdaForm fvs _ vs expr) =
  let found =
       exprFVS (S.fromList vs `union` S.fromList (map bindName binds)) expr in
  let declared = S.fromList fvs in
  (found \\ declared, declared \\ found)

type GetLFS a = a -> [LambdaForm]

getProgLFS :: GetLFS Prog
getProgLFS (MkProg binds) = getBindLFS =<< binds

getBindLFS :: GetLFS Bind
getBindLFS (MkBind _ lf _) = lf : getLFLFS lf

getLFLFS :: GetLFS LambdaForm
getLFLFS (MkLambdaForm _ _ _ expr) = getExprLFS expr

getExprLFS :: GetLFS Expr
getExprLFS (LetE binds expr _) = getExprLFS expr ++ (getBindLFS =<< binds)
getExprLFS (LetRecE binds expr _) = getExprLFS expr ++ (getBindLFS =<< binds)
getExprLFS (CaseE expr alts _) = getExprLFS expr ++ (getAltsLFS alts)
getExprLFS _ = []

getAltsLFS :: GetLFS Alts
getAltsLFS (AlgAlts alts def) =
  getDefaultAltLFS def ++ (getAlgAltLFS =<< alts)
getAltsLFS (PrimAlts alts def) =
  getDefaultAltLFS def ++ (getPrimAltLFS =<< alts)

getAlgAltLFS :: GetLFS AlgAlt
getAlgAltLFS (AAlt _ _ expr _) = getExprLFS expr

getPrimAltLFS :: GetLFS PrimAlt
getPrimAltLFS (PAlt _ expr _) = getExprLFS expr

getDefaultAltLFS :: GetLFS DefaultAlt
getDefaultAltLFS (Default expr _) = getExprLFS expr
getDefaultAltLFS (DefaultVar _ expr _) = getExprLFS expr
