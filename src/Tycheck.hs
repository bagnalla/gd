module Tycheck where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State as S
import Control.Monad.Writer
import Data.Bifunctor (second)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Ast
import Symtab (add, addBindings, empty, fromList, get, Id(..), Symtab,
               union)
import TycheckUtil
import Util

-- TODO: functions with non-void (and non-dynamic) return types must
-- return a value in every possible execution flow.

type Gamma = Symtab Type

type Namespace = Symtab Type

data TycheckState =
  TycheckState { s_parent_class :: Id
               , s_class :: Type
               , s_class_static :: Type
               , s_gensym_counter :: Int
               , s_constants :: Symtab (Expr Type)
               -- , s_vars :: Symtab Type
               , s_delta :: Symtab Type
               , s_namespaces :: Map.Map Type Namespace }

-- upd_namespace :: Type -> Id -> Type -> TycheckState -> TycheckState
-- upd_namespace s x t st@(TycheckState { s_namespaces = namespaces }) =
--   st { s_namespaces =
--        Map.insert s
--        (Symtab.add x t
--         (fromMaybe Map.empty $ Map.lookup s namespaces))
--        namespaces }

upd_constants :: Id -> Expr Type -> TycheckM ()
upd_constants x e =
  modify $ \st@(TycheckState { s_constants = consts }) ->
             st { s_constants = Symtab.add x e consts }

-- upd_vars :: Id -> Type -> TycheckM ()
-- upd_vars x e =
--   modify $ \st@(TycheckState { s_vars = vars }) ->
--              st { s_vars = Symtab.add x e vars }

upd_namespace :: Type -> Id -> Type -> TycheckM ()
upd_namespace s x t =
  modify $ \st ->
             st { s_namespaces =
                    Map.insert s
                    (Symtab.add x t
                      (fromMaybe Map.empty $
                       Map.lookup s (s_namespaces st)))
                    (s_namespaces st) }

get_namespace :: Type -> TycheckM (Maybe Namespace)
get_namespace ty = do
  namespaces <- gets s_namespaces
  return $ Map.lookup ty namespaces

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

-- isConstPattern :: Symtab (Expr Type) -> Pattern α -> Bool
-- isConstPattern γ (PLiteral lit) = isConstLiteral γ lit
-- isConstPattern γ (PIdent _) = True
-- isConstPattern γ (PVar _) = False


lookupIdent :: Show α => α -> Id -> TycheckM Type
lookupIdent fi x = do
  γ <- ask
  case Symtab.get x γ of
    Just ty -> return ty
    Nothing -> typeError fi $ "unbound identifier " ++ show x

tycheckLiteral :: Show α => α -> Literal α -> TycheckM (Type, Literal Type)
tycheckLiteral fi (LBool b) = return (TBool, LBool b)
tycheckLiteral fi (LInt i) = return (TInt, LInt i)
tycheckLiteral fi (LFloat f) = return (TFloat, LFloat f)
tycheckLiteral fi (LString s) = return (TString, LString s)
tycheckLiteral fi (LArray arr) = do
  arr' <- mapM tycheckExpr arr
  let tys = data_of_expr <$> arr'
  ty <- if length arr' == 0 then
          return TDynamic
        else
          return $ if allEq tys then tys!!0 else TDynamic
  return (TArray ty, LArray arr')
tycheckLiteral fi (LDict entries) = do
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
  return (TDict key_ty value_ty, LDict $ zip keys values)


tycheckExpr :: Show α => Expr α -> TycheckM (Expr Type)

tycheckExpr (ELiteral fi lit) = do
  (ty, lit') <- tycheckLiteral fi lit
  return $ ELiteral ty lit'

tycheckExpr (EIdent fi x) = flip EIdent x <$> lookupIdent fi x

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


assertTyExpr :: Show α => Type -> Expr α -> TycheckM (Expr Type)
assertTyExpr ty e = do
  e' <- tycheckExpr e
  let e_ty = data_of_expr e'
  if e_ty `compatible` ty then
    return e'
    else
    typeError (data_of_expr e) $
    show e_ty ++ " not compatible with " ++ show ty


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
  let ty = data_of_expr e'
  cases' <- flip mapM cases $
    \(p, body) -> do
      γ <- gets s_constants
      if patternWellFormed γ p then do
        tycheckPattern fi p ty
        let binds = pattern_binds p ty
        body' <- local (addBindings binds) $ tycheckStmts body
        return (TVoid <$ p, body')
        else
        typeError fi "ill-formed pattern"
  return $ SMatch TVoid e' cases'

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

-- TODO: after this, bind nm to enum type and bind the type to a
-- namespace containing the fields.
tycheckCommand (CEnum fi nm fields) = do
  fields' <- mapM (secondM $ mapM $ assertTyExpr TInt) fields
  return $ CEnum TVoid nm fields'

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

-- Static functions won't include member bindings in initial gamma.
tycheckCommand (CFunc fi static f ret_ty args body) = do
  class_ty <- gets s_class
  class_static_ty <- gets s_class_static
  class_namespace <- fromMaybe empty <$> get_namespace class_ty
  class_static_namespace <-
    fromMaybe empty <$> get_namespace class_static_ty
  let γ = if static then
            class_static_namespace
          else
            Symtab.union class_static_namespace class_namespace
  let γ' = addBindings args γ
  body' <- local (const γ') $ tycheckStmts body
  -- Analyze body to ensure that all return statements match the
  -- return type, and if the return type is non-void that all branches
  -- return something.
  if ret_ty /= TVoid && not (always_returns body') then
    typeError fi
    "function with non-void return type must always return something"
    else do
    check_returns fi ret_ty body'
    return $ CFunc TVoid static f ret_ty args body'

tycheckCommand (CExtends fi e) = undefined

tycheckCommand (CClass fi cls) = do
  outer_class <- gets s_class_static
  CClass TVoid <$> tycheckClass outer_class cls

tycheckCommand (CSignal _ _ _) = undefined

tycheckCommand (CClassName fi nm path) = undefined


tycheckClass :: Show α => Type -> Class α -> TycheckM (Class Type)
tycheckClass outer_class (Class { class_name = nm
                                , class_commands = coms }) = do
  let class_ty = TClass outer_class nm
  let class_static_ty = TClassStatic outer_class nm
  let f_coms = filter is_func_command coms
  let phase1_coms = filter is_phase1_command coms
  let phase2_coms = filter is_phase2_command coms
  -- let extends_coms = filter is_extends_command coms
  let f_tys = flip map f_coms $
              \(CFunc _ _ f ty args _) ->
                (f, TFunc ty $ snd <$> args)
  
  -- Add bindings for all static things (consts, enums, static
  -- methods, etc.) to the static namespace for this class, and add
  -- bindings for all member stuff (vars, functions, etc.) to the
  -- member namespace.  When typechecking the bodies of functions,
  -- initialize gamma with local bindings for all of them as well as a
  -- 'self' binding.

  -- Add types of all functions to delta.
  modify $ \s -> s { s_delta = addBindings f_tys $ s_delta s }

  -- Typecheck static stuff (constants, enums), adding their bindings
  -- to the state along the way.
  phase1_coms' <- flip mapM phase1_coms $
                 \c -> do
                   c' <- tycheckCommand c
                   case c' of
                     CConst _ x (Just ty) e -> do
                       -- Add constant binding.
                       upd_constants x e
                       -- -- Add var binding.
                       -- upd_vars x $ data_of_expr e
                       -- Add binding to class static namespace.
                       upd_namespace class_static_ty x ty
                     CEnum _ nm fields -> do
                       let field_binds = (second $ const TInt) <$> fields
                       case nm of
                         Just x -> do
                           -- If the enum is named, add a binding for
                           -- it in the class's static namespace and
                           -- add bindings for its fields in its own
                           -- namespace.
                           let enum_ty = TEnum class_static_ty x
                           upd_namespace class_static_ty x enum_ty
                           flip mapM_ field_binds $
                             \(y, ty) -> upd_namespace enum_ty y ty
                         Nothing ->
                           -- If the enum is not named, add bindings
                           -- for its fields in the class's namespace.
                           flip mapM_ field_binds $
                             \(y, ty) -> upd_namespace class_ty y ty
                   return c'

  phase2_coms' <- flip mapM phase2_coms $
               \c -> do
                 c' <- tycheckCommand c
                 case c' of
                   CVar _ _ _ _ x (Just ty) _ _ ->
                     -- Add binding to class namespace.
                     upd_namespace class_ty x ty
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

patternWellFormed :: Symtab (Expr Type) -> Pattern α -> Bool
patternWellFormed γ (PArray arr) = all (patternWellFormed γ) arr
patternWellFormed γ (PDict pats) =
  if null pats then True else
    let open_ok = not (any is_open_entry_pattern $ init pats) in
      all (\pat -> case pat of
              DEPOpen -> True
              DEPKV k v -> is_literal_pattern k && patternWellFormed γ v)
      pats
patternWellFormed γ (PMulti pats) = all (patternWellFormed γ) pats
patternWellFormed _ _ = True


tycheckPattern :: Show α => α -> Pattern α -> Type -> TycheckM ()
tycheckPattern fi (PLiteral lit) ty = do
  γ <- gets s_constants
  if isConstLiteral γ lit then do
    (lit_ty, lit') <- tycheckLiteral fi lit
    if lit_ty `compatible` ty then
      return ()
      else
      typeError fi $ show lit_ty ++ "not compatible with " ++ show ty
  else
    typeError fi $ "expected constant expression"
tycheckPattern fi (PIdent x) ty = do
  id_ty <- lookupIdent fi x
  if id_ty `compatible` ty then
    return ()
    else
    typeError fi $ show id_ty ++ " not compatible with " ++ show ty

-- Check pattern is well-formed before using this.
tycheckPattern _ _ TDynamic = return()
tycheckPattern _ (PVar _) _ = return ()
tycheckPattern _ (PWildcard) _ = return ()
tycheckPattern fi (PArray arr) (TArray ty) =
  mapM_ (flip (tycheckPattern fi) ty) arr
tycheckPattern fi (PArray arr) ty =
  typeError fi $ "expected array type. actual: " ++ show ty
tycheckPattern fi (PDict pats) (TDict k_ty v_ty) =
  mapM_ (\pat -> case pat of
            DEPOpen -> return ()
            DEPKV k v -> tycheckPattern fi k k_ty >>
                         tycheckPattern fi v v_ty)
  pats
tycheckPattern fi (PDict pats) ty =
  typeError fi $ "expected dict type. actual: " ++ show ty
tycheckPattern fi (PMulti pats) ty =
  mapM_ (flip (tycheckPattern fi) ty) pats


pattern_binds :: Pattern α -> Type -> [(Id, Type)]
pattern_binds (PVar x) ty = [(x, ty)]
pattern_binds (PArray ps) (TArray t) =
  concat $ flip pattern_binds t <$> ps
pattern_binds (PDict ps) (TDict k v) =
  concat $ flip dictentry_pattern_binds v <$> ps
pattern_binds _ _ = []

dictentry_pattern_binds :: DictEntryPattern α -> Type -> [(Id, Type)]
dictentry_pattern_binds DEPOpen _ = []
dictentry_pattern_binds (DEPKV k v) ty = pattern_binds k ty


-- TODO: split this in to two parts. 1) check that all returns in the
-- body match the return type (in the monad), and 2) check that the
-- function always returns (simple predicate). For 2) we can basically
-- ignore loops. At if branches, if all branches always return
-- (recursive call) then we are done, otherwise we continue looking
-- for a return.

-- TODO: check that breaks and continues don't occur outside of loops.

always_returns :: [Stmt α] -> Bool
always_returns [] = False
always_returns (SIf _ _ if_branch elifs else_branch : stmts) =
  if always_returns if_branch &&
  (all always_returns (snd <$> elifs)) &&
  fromMaybe True (always_returns <$> else_branch) then
    True
  else
    always_returns stmts
always_returns (SReturn _ _ : _) = True
-- always_returns (SBreak _ : _) = False
-- always_returns (SContinue _ : _) = False
always_returns (_ : stmts) = always_returns stmts


-- TODO: just check that all return expressions' types are compatible
-- with the argument type.
check_returns :: Show α => α -> Type -> [Stmt Type] -> TycheckM ()
check_returns fi ty body = undefined

-- data Stmt α =
--   SPass α
--   | SVar α Id (Maybe Type) (Maybe (Expr α))
--   | SIf α (Expr α) [Stmt α] [(Expr α, [Stmt α])] (Maybe [Stmt α])
--   | SWhile α (Expr α) [Stmt α]
--   | SFor α Id (Expr α) [Stmt α]
--   -- Can have multiple patterns separated by commas (not allowed to be
--   -- binding patterns in that case)
--   | SMatch α (Expr α) [(Pattern α, [Stmt α])]
--   | SBreak α
--   | SContinue α
--   | SAssert α (Expr α)
--   | SReturn α (Expr α)
--   | SExpr α (Expr α)
--   deriving (Eq, Functor)