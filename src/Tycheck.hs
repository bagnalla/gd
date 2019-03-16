module Tycheck where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State as S
import Control.Monad.Writer
import qualified Data.Map as Map

import Ast
import Symtab (add, addBindings, empty, fromList, get, Id(..), Symtab)
import TycheckUtil
import Util

-- TODO: functions with non-void (and non-dynamic) return types must
-- return a value in every possible execution flow.

type Gamma = Symtab Type

-- type FuncType = (Type, [Type])

-- Static (consts and static methods) and member (vars and member
-- methods) namespaces.
type Namespaces = (Symtab Type, Symtab Type)

data TycheckState =
  TycheckState { s_parent_class :: Id
               , s_gensym_counter :: Int
               , s_constants :: Symtab (Expr Type)
               , s_vars :: Symtab Type
               , s_delta :: Symtab Type
               , s_namespaces :: Map.Map Type Namespaces }

----------------
-- | Typechecker

type TycheckM a =
  WriterT [String]
  (ReaderT Gamma
   (ExceptT String
    (StateT TycheckState Identity))) a

nextSym :: String -> TycheckM String
nextSym prefix = do
  i <- S.gets s_gensym_counter
  modify $ \s -> s { s_gensym_counter = i + 1 }
  return $ prefix ++ show i

runTycheck :: TycheckState -> Id -> TycheckM a ->
              Either String (a, [String])
runTycheck s mod_name =
  fst . runIdentity . flip runStateT s . runExceptT .
  flip runReaderT empty . runWriterT

typeError :: Show α => α -> String -> TycheckM b
typeError fi msg =
  throwError $ "Type error: " ++ msg ++ " at " ++ show fi


isConstLiteral :: Symtab (Expr Type) -> Literal α -> Bool
isConstLiteral _ (LBool _)   = True
isConstLiteral _ (LInt _)    = True
isConstLiteral _ (LFloat _)  = True
isConstLiteral _ (LString _) = True
isConstLiteral γ (LArray arr) = all (isConstExpr γ) arr
isConstLiteral γ (LDict entries) =
  let (keys, values) = unzip entries in
    all (isConstExpr γ) keys && all (isConstExpr γ) values

-- γ is for names of declared constants and things like PI.
isConstExpr :: Symtab (Expr Type) -> Expr α -> Bool
isConstExpr γ (ELiteral _ lit) = isConstLiteral γ lit
isConstExpr γ (EIdent _ x) =
  case Symtab.get x γ of
    Just _  -> True
    Nothing -> False
isConstExpr γ (ECall _ f args) =
  isConstFunc f && all (isConstExpr γ) args
isConstExpr γ (EUnop _ _ e) = isConstExpr γ e 
isConstExpr γ (EBinop _ _ e1 e2) = isConstExpr γ e1 && isConstExpr γ e2
isConstExpr _ (EIfElse _ _ _ _) = False

isConstFunc :: Expr α -> Bool
isConstFunc f =
  case f of
    EIdent _ (Id x) ->
      x `elem` ["sin", "cos"] -- TODO: builtins, constructors, etc.
    _ -> False


tycheckExpr :: Show α => Expr α -> TycheckM (Expr Type)

tycheckExpr (ELiteral _ (LBool b)) = return $ ELiteral TBool (LBool b)
tycheckExpr (ELiteral _ (LInt i)) = return $ ELiteral TInt (LInt i)
tycheckExpr (ELiteral _ (LFloat f)) = return $ ELiteral TFloat (LFloat f)
tycheckExpr (ELiteral _ (LString s)) = return $ ELiteral TString (LString s)
tycheckExpr (ELiteral fi (LArray arr)) = do
  arr' <- mapM tycheckExpr arr
  let tys = data_of_expr <$> arr'
  ty <- if length arr' == 0 then
          return TDynamic
        else
          return $ if allEq tys then tys!!0 else TDynamic
  return $ ELiteral (TArray ty) $ LArray arr'
tycheckExpr (ELiteral fi (LDict entries)) = do
  (keys, values) <- unzip <$> mapM (bimapM' tycheckExpr) entries
  let key_tys = data_of_expr <$> keys
  let value_tys = data_of_expr <$> values
  key_ty <- if length keys == 0 then
              return TDynamic
            else
              return $ if allEq key_tys then key_tys!!0 else TDynamic
  value_ty <- if length values == 0 then
                return TDynamic
              else
                return $ if allEq value_tys then value_tys!!0 else TDynamic
  return $ ELiteral (TDict key_ty value_ty) $ LDict $ zip keys values

tycheckExpr (EIdent fi x) = do
  γ <- ask
  case Symtab.get x γ of
    Just ty -> return $ EIdent ty x
    Nothing -> typeError fi $ "unbound variable " ++ show x

tycheckExpr (ECall fi f args) = do
  δ <- gets s_delta
  f' <- tycheckExpr f
  let f_ty = data_of_expr f'
  case f_ty of
    TFunc ret_ty arg_tys ->
      if length args == length arg_tys then do
        args' <- flip mapM (zip args arg_tys) $
                 \(arg, expected) -> do
                   arg' <- tycheckExpr arg
                   let actual = data_of_expr arg'
                   if compatible actual expected then return arg'
                     else
                     typeError (data_of_expr arg) $
                     "incompatible argument type. expected: " ++
                     show expected ++ ", actual: " ++ show actual
        return $ ECall ret_ty f' args'
      else
        typeError fi $ "wrong number of arguments to function " ++ show f
        ++ ". expected: " ++ show (length arg_tys) ++ ", actual: "
        ++ show (length args)
    _ -> typeError fi $ "expected function type. actual: " ++ show f_ty

tycheckExpr (EUnop fi UBitwiseNot e) = do
  e' <- tycheckExpr e
  let ty = data_of_expr e'
  if ty == TInt then
    return $ EUnop TInt UBitwiseNot e'
    else
    typeError fi $ "wrong type. expected: " ++
    show TInt ++ ", actual: " ++ show ty

tycheckExpr (EUnop fi UNeg e) = do
  e' <- tycheckExpr e
  let ty = data_of_expr e'
  if is_numeric ty then
    return $ EUnop ty UNeg e'
    else
    typeError fi $ "wrong type. expected: numeric, actual: " ++ show ty

tycheckExpr (EUnop fi UNot e) = do
  e' <- tycheckExpr e
  let ty = data_of_expr e'
  if ty == TBool || is_numeric ty then
    return $ EUnop ty UNot e'
    else
    typeError fi $ "wrong type. expected: bool or numeric, actual: "
    ++ show ty

tycheckExpr (EUnop fi UGetNode e) = do
  if string_or_ident e then
    return $ EUnop TObject UGetNode $ TDynamic <$ e
    else
    typeError fi $ "expected string literal or identifier after '$'"

-- TODO: look up attribute in parent class. If parent class is unknown
-- (missing 'extends' command or just an unknown type) then error.
tycheckExpr (EUnop fi UAttribute e) = do
  case e of
    EIdent _ x -> undefined
    _ -> typeError fi $ ""

tycheckExpr (EBinop fi b e1 e2) = do
  e1' <- tycheckExpr e1
  e2' <- tycheckExpr e2
  let t1 = data_of_expr e1'
  let t2 = data_of_expr e2'
  if t1 == TDynamic && t2 == TDynamic then
    return $ EBinop TDynamic b e1' e2'
    else if numeric_binop b then
    if is_numeric t1 && is_numeric t2 then
      return $ EBinop (numeric_result t1 t2) b e1' e2'
    else
      typeError fi $ "expected numeric types. actual: "
      ++ show t1 ++ ", " ++ show t2
    else if int_binop b then
    if t1 == TInt && t2 == TInt then
      return $ EBinop TInt b e1' e2'
    else
      typeError fi $ "expected ints. actual: "
      ++ show t1 ++ ", " ++ show t2
    else if same_type_binop b then
    -- Either same type or both numeric (ints are comparable with
    -- floats).
    if t1 == t2 || is_numeric t1 && is_numeric t2 then
      return $ EBinop TBool b e1' e2'
    else
      typeError fi $ "expected operands to have same types. actual: "
      ++ show t1 ++ ", " ++ show t2
    else if bool_binop b then
    if t1 == TBool && t2 == TBool then
      return $ EBinop TBool b e1' e2'
    else
      typeError fi $ "expected bools. actual: "
      ++ show t1 ++ ", " ++ show t2
    else
    case b of
      BIs ->
        case e2' of
          EType _ _ ->
            return $ EBinop TBool b e1' e2'
          _ -> typeError fi ""
      BIn -> -- Allow compatibility in both directions? Yes for now.
        case t2 of
          TArray t ->
            if compatible t1 t || compatible t t1 then
              return $ EBinop TBool b e1' e2'
            else
              typeError fi $ "incompatible types: "
              ++ show t1 ++ " and " ++ show t
          TDict t _ ->
            if compatible t1 t || compatible t t1 then
              return $ EBinop TBool b e1' e2'
            else
              typeError fi $ "incompatible types: "
              ++ show t1 ++ " and " ++ show t
          _ ->
            typeError fi $
            "expected array or dict type. actual: " ++ show t2
      BIndex ->
        case t1 of
          TArray t ->
            if t2 == TInt then
              return $ EBinop t b e1' e2'
            else
              typeError fi $ "expected int index. actual: " ++ show t2
          TDict k v ->
            if t2 == k then
              return $ EBinop v b e1' e2'
            else
              typeError fi $ "wrong dict index type. expected: "
              ++ show k ++ ", actual: " ++ show t2
          TDynamic ->
            return $ EBinop TDynamic b e1' e2'
          _ -> typeError fi $ "expected array or dict type. actual: "
               ++ show t1
      BAttribute ->
        -- TODO: may need to avoid typechecking e2 (move this case to
        -- the front).
        -- TODO: look up the identifier in e2 in the namespace of e1's
        -- type.
        case e2 of
          EIdent _ x ->
            -- TODO: look up x in the namespace of e1's type.
            let t = undefined in
              return $ EBinop t b e1' $ TDynamic <$ e2
          _ -> typeError fi ""
      BAssign ->
        if t2 `compatible` t1 then
          return $ EBinop t1 b e1' e2'
        else
          typeError fi $ show t2 ++ " is not compatible with " ++ show t1
      _ -> typeError fi
           "failed to match binop in tycheckExpr. should be impossible"

tycheckExpr (EIfElse fi e1 e2 e3) = do
  e1' <- tycheckExpr e1
  e2' <- tycheckExpr e2
  e3' <- tycheckExpr e3
  let t1 = data_of_expr e1'
  let t2 = data_of_expr e2'
  let t3 = data_of_expr e3'
  if t1 == TDynamic && t2 == TDynamic && t3 == TDynamic then
    return $ EIfElse TDynamic e1' e2' e3'
    else
    if t1 == TBool then
      if t2 `compatible` t3 && t3 `compatible` t2 then
        return $ EIfElse (join_types t2 t3) e1' e2' e3'
      else
        typeError fi $ "branches of if/else have incompatible types: "
        ++ show t2 ++ ", " ++ show t3
    else
      typeError fi $ "expected bool. actual: " ++ show t1

tycheckExpr (EType _ t) = return $ EType TDynamic t


tycheckStmt :: Show α => Stmt α -> TycheckM (Stmt Type)

tycheckStmt (SPass _) = return $ SPass TNull

-- When typechecking statements, extend gamma with the type computed
-- here (in the data slot).
tycheckStmt (SVar fi x ty init_val) = do
  init_val' <- mapM tycheckExpr init_val
  let init_ty = data_of_expr <$> init_val'
  case (ty, init_ty) of
    (Nothing, Just t) ->
      return $ SVar t x ty init_val'
    (Just s, Just t) ->
      if t `compatible` s then
        return $ SVar s x ty init_val'
      else
        typeError fi $ show t ++ " not compatible with " ++ show s
    (Just s, Nothing) ->
      return $ SVar s x ty init_val'
    (Nothing, Nothing) ->
      typeError fi "unable to infer type"

tycheckStmt (SIf fi discrim body elifs els) = do
  discrim' <- tycheckExpr discrim
  body' <- tycheckStmts body
  elifs' <- mapM (bimapM tycheckExpr tycheckStmts) elifs
  els' <- mapM tycheckStmts els
  return $ SIf TVoid discrim' body' elifs' els'

tycheckStmt (SWhile fi discrim body) = do
  discrim' <- tycheckExpr discrim
  body' <- tycheckStmts body
  return $ SWhile TVoid discrim' body'

tycheckStmt (SFor fi x e body) = do
  e' <- tycheckExpr e
  body' <- local (Symtab.add x $ data_of_expr e') $ tycheckStmts body
  return $ SFor TVoid x e' body'

  -- | SMatch α (Expr α) [(Pattern α, [Stmt α])]
tycheckStmt (SMatch fi e cases) = do
  e' <- tycheckExpr e
  cases' <- flip mapM cases $
    \(p, body) -> do
      body' <-
        case p of
          PVar x -> local (Symtab.add x $ data_of_expr e') $
                    tycheckStmts body
          PArray ps -> undefined -- TODO
          PDict ps -> undefined
          _ -> tycheckStmts body
      return (p, body')
  undefined

tycheckStmt (SBreak _) = return $ SBreak TVoid
tycheckStmt (SContinue _) = return $ SContinue TVoid

tycheckStmt (SAssert _ e) = pure (SAssert TVoid) <*> tycheckExpr e
tycheckStmt (SReturn _ e) = pure (SReturn TVoid) <*> tycheckExpr e
tycheckStmt (SExpr _   e) = pure (SExpr   TVoid) <*> tycheckExpr e


tycheckStmts :: Show α => [Stmt α] -> TycheckM [Stmt Type]
tycheckStmts (stmt:stmts) = do
  stmt' <- tycheckStmt stmt
  stmts' <- case stmt' of
              SVar ty x _ _ ->
                local (Symtab.add x ty) $ tycheckStmts stmts
              _ -> tycheckStmts stmts
  return $ stmt' : stmts'
tycheckStmts [] = return []


tycheckCommand :: Show α => Command α -> TycheckM (Command Type)

tycheckCommand (CEnum fi nm fields) = undefined

tycheckCommand (CVar fi _ _ _ x ty init_val setget) = undefined

tycheckCommand (CConst fi x ty e) = do
  γ <- gets s_constants
  if isConstExpr γ e then do
    e' <- tycheckExpr e
    let t = data_of_expr e'
    case ty of
      Just ty' ->
        if t `compatible` ty' then
          return $ CConst TVoid x ty e'
        else
          typeError fi $ show t ++ " is not compatible with " ++ show ty'
      Nothing ->
        return $ CConst TVoid x (Just t) e'
    else
    typeError fi "expected a constant expression"

-- Static functions won't include s_vars from the tycheck state in the
-- initial gamma.
tycheckCommand (CFunc fi static f ret_ty args body) = undefined

tycheckCommand (CExtends fi e) = undefined

tycheckCommand (CClass fi cls) = CClass TVoid <$> tycheckClass cls

tycheckCommand (CSignal _ _ _) = undefined

tycheckCommand (CClassName fi nm path) = undefined


tycheckClass :: Show α => Class α -> TycheckM (Class Type)
tycheckClass (Class { class_name = nm
                    , class_commands = coms }) = do
  let f_coms = filter is_func_command coms
  let const_coms = filter is_const_command coms
  let var_coms = filter is_var_command coms
  let extends_coms = filter is_extends_command coms
  let f_tys = flip map f_coms $
              \(CFunc _ _ f ty args _) ->
                (f, TFunc ty $ snd <$> args)

  -- TODO: add namespaces for the class being defined and add all of
  -- the functions to them. Also add a binding of 'self' to that type
  -- in gamma.

  -- Add types of all functions to delta.
  modify $ \s -> s { s_delta = addBindings f_tys $ s_delta s }

  -- Typecheck constants, adding their expressions and types to the
  -- state along the way.
  const_coms' <- flip mapM const_coms $
                 \c -> do
                   c' <- tycheckCommand c
                   case c' of
                     CConst _ x (Just ty) e -> do
                       modify $
                         \s ->
                           s { s_constants = Symtab.add x e (s_constants s)
                             , s_vars = Symtab.add x ty (s_vars s) }
                       return c'
                       
  var_coms' <- flip mapM const_coms $
               \c -> do
                 c' <- tycheckCommand c
                 case c' of
                   CVar _ _ _ _ x (Just ty) e _ -> do
                     modify $
                       \s ->
                         s { s_vars = Symtab.add x ty (s_vars s) }
                     return c'
  
  undefined

-- data Class α = Class { class_name :: Id
--                      , class_commands :: [Command α] }
--   deriving (Eq, Functor)

-- data Command α = 
--   CEnum α (Maybe Id) [(Id, Maybe (Expr α))]
--   -- export, export list, onready, name, type, initial value, setget
--   | CVar α Bool (Maybe (Type, [Expr α])) Bool Id (Maybe Type) (Maybe (Expr α))
--     (Maybe (Id, Id))
--   | CConst α Id (Expr α)
--   -- static, name, return type, args, body
--   | CFunc α Bool Id Type [(Id, Type)] [Stmt α]
--   | CExtends α (Expr α)
--   | CClass α (Class α)
--   -- name, args
--   | CSignal α Id [Id]
--   | CClassName α Id (Maybe String)
--   deriving (Eq, Functor)
