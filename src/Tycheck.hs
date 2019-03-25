module Tycheck where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State as S
import Control.Monad.Writer
import Data.Aeson (Value)
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)

import Ast
import GodotApi (parse_api_namespaces)
import Namespace (Namespace)
import qualified Namespace as NS
import Symtab (add, addBindings, empty, fromList, toList,
               get, Id(..), Symtab, union)
import TycheckUtil
import Util


-- Type of local context (separate contexts γ for variables and δ for
-- functions).
data Context = Context { ctx_gamma :: Symtab Type
                       , ctx_delta :: Symtab Type
                       , ctx_expecting_fun :: Bool
                       , ctx_locals :: [Id]
                       -- If true, don't look in member namespace of
                       -- the current class.
                       , ctx_static :: Bool}

empty_ctx :: Context
empty_ctx = Context { ctx_gamma = Symtab.empty
                    , ctx_delta = Symtab.empty
                    , ctx_expecting_fun = False
                    -- Track locals declared to prevent shadowing.
                    , ctx_locals = []
                    , ctx_static = False }

upd_gamma :: (Symtab Type -> Symtab Type) -> Context -> Context
upd_gamma f ctx@(Context { ctx_gamma = gamma }) =
  ctx { ctx_gamma = f gamma }

upd_delta :: (Symtab Type -> Symtab Type) -> Context -> Context
upd_delta f ctx@(Context { ctx_delta = delta }) =
  ctx { ctx_delta = f delta }

upd_locals :: ([Id] -> [Id]) -> Context -> Context
upd_locals f ctx@(Context { ctx_locals = locals }) =
  ctx { ctx_locals = f locals }

set_static :: Bool -> Context -> Context
set_static b ctx = ctx { ctx_static = b }

mkCtx :: Symtab Type -> Symtab Type -> Context
mkCtx γ δ = upd_gamma (const γ) (upd_delta (const δ) empty_ctx)


-- Typechecker state.
data TycheckState =
  TycheckState { -- s_parent_class :: Maybe Type
                 s_class :: Type
               , s_class_static :: Type
               , s_constants :: Symtab (Expr Type)
               -- -- Global constants.
               -- , s_global_vars :: Symtab Type
               -- -- Global functions (including constructors for builtin types).
               -- , s_global_funs :: Symtab Type
               -- Map types to their parent types (void if none).
               , s_parents :: Symtab Type
               , s_namespaces :: Symtab Namespace }

-- -- This expecting_fun thing is a bit of a hack that hopefully won't be
-- -- necessary at all in the future when first-class functions are added
-- -- to GDScript (assuming variable and function namespaces are merged
-- -- into one). Basically, when typechecking a function call expression,
-- -- we set this flag before typechecking the LHS so that it knows to
-- -- look it up in the function namespace.
-- set_expecting_fun :: Bool -> TycheckM ()
-- set_expecting_fun b = modify $ \s -> s { s_expecting_fun = b }

upd_constants :: Id -> Expr Type -> TycheckM ()
upd_constants x e =
  modify $ \st@(TycheckState { s_constants = consts }) ->
             st { s_constants = Symtab.add x e consts }

add_empty_namespace :: Show α => α -> Type -> [Id] -> TycheckM ()
add_empty_namespace fi ty ty_params = do
  let ty_name = type_name ty
  st <- S.get
  case Symtab.get ty_name (s_namespaces st) of
    Nothing ->
      S.put $ st
      { s_namespaces =
          Symtab.add ty_name (NS.new ty_params) (s_namespaces st) }
    Just ns -> do
      typeError fi $ "namespace already exists for " ++ show ty

upd_var_namespace :: Show α => α -> Type -> Id -> Type -> TycheckM ()
upd_var_namespace fi s x t = do
  let s_name = type_name s
  st <- S.get
  case Symtab.get s_name (s_namespaces st) of
    Just ns ->
      S.put $ st { s_namespaces =
                   Symtab.add s_name (NS.add_var x t ns) (s_namespaces st) }
    Nothing -> typeError fi $ "namespace not found for " ++ show s

upd_fun_namespace :: Show α => α -> Type -> Id -> Type -> TycheckM ()
upd_fun_namespace fi s x t = do
  let s_name = type_name s
  st <- S.get
  case Symtab.get s_name (s_namespaces st) of
    Just ns ->
      S.put $ st { s_namespaces =
                   Symtab.add s_name (NS.add_fun x t ns) (s_namespaces st) }
    Nothing -> typeError fi $ "namespace not found for " ++ show s

get_namespace :: Type -> TycheckM (Maybe Namespace)
get_namespace ty = do
  namespaces <- gets s_namespaces
  return $ Symtab.get (type_name ty) namespaces

get_namespace_error :: Show α => α -> Type -> TycheckM Namespace
get_namespace_error fi ty = do
  namespaces <- gets s_namespaces
  case Symtab.get (type_name ty) namespaces of
    Just ns -> return ns
    Nothing -> typeError fi $ "unknown type " ++ show ty

_lookup_ns :: ((Symtab Type, Symtab Type) -> Symtab Type) ->
              Type -> Id -> TycheckM (Maybe Type)
_lookup_ns proj ty x = do
  ns <- get_namespace ty
  return $ ns >>= NS.instantiate_namespace (type_args ty) >>=
    Symtab.get x . proj

_lookup_ns_deep :: ((Symtab Type, Symtab Type) -> Symtab Type) ->
                   Type -> Id -> TycheckM (Maybe Type)
_lookup_ns_deep proj ty x = do
  t <- _lookup_ns proj ty x
  case t of
    Just t' -> return t
    Nothing -> do
      parent <- get_parent_class ty
      case parent of
        Just par -> _lookup_ns_deep proj par x
        Nothing -> return Nothing

_lookup_ns_deep_error :: Show α => ((Symtab Type, Symtab Type) -> Symtab Type) ->
                      α -> Type -> Id -> TycheckM Type
_lookup_ns_deep_error proj fi ty x = do
  t <- _lookup_ns_deep proj ty x
  case t of
    Just t' -> return t'
    Nothing -> typeError fi $ "attribute " ++ show x ++
      " not found in " ++ show ty ++ " variable namespace"

lookup_var :: Type -> Id -> TycheckM (Maybe Type)
lookup_var = _lookup_ns fst

lookup_var_deep :: Type -> Id -> TycheckM (Maybe Type)
lookup_var_deep = _lookup_ns_deep fst

lookup_var_deep_error :: Show α => α -> Type -> Id -> TycheckM Type
lookup_var_deep_error = _lookup_ns_deep_error fst

lookup_fun :: Type -> Id -> TycheckM (Maybe Type)
lookup_fun = _lookup_ns snd

lookup_fun_deep :: Type -> Id -> TycheckM (Maybe Type)
lookup_fun_deep = _lookup_ns_deep snd

lookup_fun_deep_error :: Show α => α -> Type -> Id -> TycheckM Type
lookup_fun_deep_error = _lookup_ns_deep_error snd

_lookup_ns_error :: Show α => ((Symtab Type, Symtab Type) -> Symtab Type) ->
                    String -> α -> Type -> Id -> TycheckM Type
_lookup_ns_error proj str fi ty x = do
  ns <- get_namespace ty
  case ns of
    Just ns' ->
      case NS.instantiate_namespace (type_args ty) ns' of
        Just p ->
          case Symtab.get x $ proj p of
            Just ty' -> return ty'
            Nothing ->
              typeError fi $ "attribute " ++ show x ++ " not bound in "
              ++ show ty ++ " " ++ str ++ " namespace"
        Nothing ->
          typeError fi $ "failed to instantiate namespace for " ++ show ty
    Nothing ->
      typeError fi $ "unknown type " ++ show ty

lookup_var_error :: Show α => α -> Type -> Id -> TycheckM Type
lookup_var_error = _lookup_ns_error fst "variable"

lookup_fun_error :: Show α => α -> Type -> Id -> TycheckM Type
lookup_fun_error = _lookup_ns_error snd "function" 


get_parent_class :: Type -> TycheckM (Maybe Type)
get_parent_class ty = do
  parents <- gets s_parents
  return $ Symtab.get (type_name ty) parents

get_parent_class_error :: Show α => α -> Type -> TycheckM Type
get_parent_class_error fi ty = do
  parents <- gets s_parents
  case Symtab.get (type_name ty) parents of
    Just par -> return par
    Nothing -> typeError fi $ "no parent class found for " ++ show ty

----------------
-- | Typechecker

type TycheckM a =
  WriterT [String]
  (ReaderT Context
   (ExceptT String
    (StateT TycheckState Identity))) a

-- nextSym :: String -> TycheckM String
-- nextSym prefix = do
--   i <- S.gets s_gensym_counter
--   modify $ \s -> s { s_gensym_counter = i + 1 }
--   return $ prefix ++ show i

runTycheck :: TycheckState -> Context -> TycheckM a ->
              Either String (a, [String])
runTycheck s ctx =
  fst . runIdentity . flip runStateT s . runExceptT .
  flip runReaderT ctx . runWriterT

typeError :: Show α => α -> String -> TycheckM b
typeError fi msg =
  throwError $ "Type error: " ++ msg ++ " at " ++ show fi


-- If not found in gamma, do deep search on class namespace (only
-- static if ctx_static is set). Search for functions in the same way.
lookupIdent :: Show α => α -> Id -> TycheckM Type
lookupIdent fi x = do
  expecting_fun <- asks ctx_expecting_fun
  static <- asks ctx_static
  class_ty <- gets s_class
  class_static_ty <- gets s_class_static
  if expecting_fun then do
    t <- lookup_fun_deep class_static_ty x
    case t of
      Just t' -> return t'
      Nothing -> do
        δ <- asks ctx_delta
        if not static then do
          t' <- lookup_fun_deep class_ty x
          case t' of
            Just t'' -> return t''
            Nothing -> do
              case Symtab.get x δ of
                Just t' -> return t'
                Nothing ->
                  typeError fi $ "unbound function identifier " ++ show x
          else do
          case Symtab.get x δ of
            Just t' -> return t'
            Nothing ->
              typeError fi $ "unbound function identifier " ++ show x 
    else do
    t <- lookup_var_deep class_static_ty x
    case t of
      Just t' -> return t'
      Nothing -> do
        γ <- asks ctx_gamma
        if not static then do
          t' <- lookup_var_deep class_ty x
          case t' of
            Just t'' -> return t''
            Nothing -> do
              case Symtab.get x γ of
                Just t' -> return t'
                Nothing ->
                  typeError fi $ "unbound variable identifier " ++ show x
          else do
          case Symtab.get x γ of
            Just t' -> return t'
            Nothing ->
              typeError fi $ "unbound variable identifier " ++ show x

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
  f' <- local (\ctx -> ctx { ctx_expecting_fun = True } ) $ tycheckExpr f
  let f_ty = data_of_expr f'
  args' <- mapM tycheckExpr args
  case f_ty of
    TDynamic ->
      return $ ECall TDynamic f' args'
    TFunc overloads -> do
      ret_ty <- tycheck_args fi f args' overloads
      return $ ECall ret_ty f' args'
    _ ->
      typeError fi $ "expected function type. actual: " ++ show f_ty

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
    return $ EUnop (TClass TVoid $ Id "Node") UGetNode $ TDynamic <$ e
    else
    typeError fi $ "expected string literal or identifier after '$'"

-- Look up attribute in parent class. If parent class is unknown
-- (missing 'extends' command or just an unknown type) then error.
tycheckExpr (EUnop fi UAttribute e) = do
  expecting_fun <- asks ctx_expecting_fun
  parent_class <- gets s_class >>= get_parent_class_error fi
  parent_class_static <- gets s_class_static >>= get_parent_class_error fi
  case e of
    EIdent _ x -> do
      let lookup = if expecting_fun then lookup_fun_deep else lookup_var_deep
      ty <- lookup parent_class x
      case ty of
        Just ty' ->
          return $ EUnop ty' UAttribute $ EIdent TVoid x
        Nothing -> do
          ty' <- lookup parent_class_static x
          case ty' of
            Just ty'' ->
              return $ EUnop ty'' UAttribute $ EIdent TVoid x
            Nothing ->
              typeError fi $
              "attribute " ++ show x ++ " not found in parent namespace"
    _ -> typeError fi $
         "expected identifier in attribute slot. actual: " ++ show e

tycheckExpr (EBinop fi b e1 e2) = do
  e1' <- local (\ctx -> ctx { ctx_expecting_fun = False }) $ tycheckExpr e1
  let t1 = data_of_expr e1'
  case b of
    BAttribute ->
      case e2 of
        EIdent _ x -> do
          expecting_fun <- asks ctx_expecting_fun
          -- Look up x in the namespace of e1's type.
          if t1 == TDynamic then
            return $ EBinop TDynamic b e1' $ TDynamic <$ e2
            else do
            expecting_fun <- asks ctx_expecting_fun
            t <- (if expecting_fun then lookup_fun_deep_error
                  else lookup_var_deep_error) fi t1 x
            return $ EBinop t b e1' $ TDynamic <$ e2
        _ -> typeError fi "expected identifier"
    _ -> do
      e2' <- tycheckExpr e2
      let t2 = data_of_expr e2'
      if t1 == TDynamic && t2 == TDynamic then
        return $ EBinop TDynamic b e1' e2'
        else if numeric_binop b then
        if is_numeric t1 && is_numeric t2 then
          return $ EBinop (numeric_result t1 t2) b e1' e2'
        else
          -- if assign_binop b && t1 == TDynamic then
          if t1 == TDynamic || (not (assign_binop b) && t2 == TDynamic) then
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
        else if t1 == TDynamic || t2 == TDynamic then
          return $ EBinop TDynamic b e1' e2'
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
            -- TODO: need to map the expression to a type somehow
            typeError fi "'is' operator not supported yet"
          -- case e2' of
          --   EType _ _ ->
          --     return $ EBinop TBool b e1' e2'
          --   _ -> typeError fi ""
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


assertTyExpr :: Show α => Type -> Expr α -> TycheckM (Expr Type)
assertTyExpr ty e = do
  e' <- tycheckExpr e
  let e_ty = data_of_expr e'
  if e_ty `compatible` ty then
    return e'
    else
    typeError (data_of_expr e) $
    show e_ty ++ " not compatible with " ++ show ty


-- Check that a type is known / well-formed.
check_ty :: Show α => α -> Type -> TycheckM ()
check_ty fi (TArray ty) = check_ty fi ty
check_ty fi (TDict k v) = check_ty fi k >> check_ty fi v
check_ty fi ty@(TClass outer x) =
  check_ty fi outer >> get_namespace_error fi ty >> return ()
check_ty fi ty@(TClassStatic outer x) =
  check_ty fi outer >> get_namespace_error fi ty >> return ()
check_ty fi ty@(TEnum outer x) =
  check_ty fi outer >> get_namespace_error fi ty >> return ()
check_ty fi (TFunc overloads) =
  mapM_ (bimapM (check_ty fi) (mapM $ check_ty fi)) overloads
check_ty _ _ = return ()


tycheckStmt :: Show α => Stmt α -> TycheckM (Stmt Type)

tycheckStmt (SPass _) = return $ SPass TNull

-- When typechecking statements, extend gamma with the type computed
-- here (in the data slot).
tycheckStmt (SVar fi x ty init_val) = do
  locals <- asks ctx_locals
  if x `elem` locals then
    typeError fi $ show x ++ " already bound in local scope"
    else do
    mapM_ (check_ty fi) ty
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
  case data_of_expr e' of
    TArray ty -> do
      body' <- local (upd_gamma $ Symtab.add x ty) $ tycheckStmts body
      return $ SFor TVoid x e' body'
    TDynamic -> do
      body' <- local (upd_gamma $ Symtab.add x TDynamic) $ tycheckStmts body
      return $ SFor TVoid x e' body'
    _ ->
      typeError fi $ "expected array type"

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
        body' <- local (upd_gamma $ addBindings binds) $ tycheckStmts body
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
                local (upd_locals (x:) . (upd_gamma $ Symtab.add x ty)) $
                tycheckStmts stmts
              _ -> tycheckStmts stmts
  return $ stmt' : stmts'
tycheckStmts [] = return []


tycheckCommand :: Show α => Command α -> TycheckM (Command Type)

tycheckCommand (CEnum fi nm fields) = do
  fields' <- mapM (secondM $ mapM $ assertTyExpr TInt) fields
  return $ CEnum TVoid nm fields'

tycheckCommand (CVar fi export export_list onready x ty init_val setget) = do
  mapM_ (firstM $ check_ty fi) export_list
  mapM_ (check_ty fi) ty
  export_list' <- flip mapM export_list $ secondM (mapM tycheckExpr)
  init_val' <- mapM tycheckExpr init_val
  ty' <- case (ty, init_val') of
           -- If wanting to infer type then there must be an
           -- initializer (should be enforced by the parser).
           (Nothing, Nothing) ->
             typeError fi "no expression to infer type from"
           -- Take the type of the initial expression.
           (Nothing, Just val) ->
             return $ data_of_expr val
           -- Take the declared type.
           (Just ty', Nothing) -> return ty'
           -- Make sure the initial expression is compatible with the
           -- declared type.
           (Just ty', Just val) ->
             if data_of_expr val `compatible` ty' then
               return ty'
             else
               typeError fi $ show (data_of_expr val) ++
               " not compatible with " ++ show ty'
  -- TODO: if setget, make sure the functions exist and have
  -- compatible types.
  return $
    CVar TVoid export export_list' onready x (Just ty') init_val' setget

tycheckCommand (CConst fi x ty e) = do
  mapM_ (check_ty fi) ty
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

-- Static functions don't include member bindings in initial gamma.
tycheckCommand (CFunc fi static f ret_ty args body) = do
  check_ty fi ret_ty
  mapM_ (secondM $ check_ty fi) args
  let γ = fromList args
  body' <- local (upd_gamma (γ `Symtab.union`)) $ tycheckStmts body
  if no_breaks_continues body' then
    -- Analyze body to ensure that all return statements match the
    -- return type, and if the return type is non-void that all branches
    -- return something.
    if ret_ty /= TVoid && ret_ty /= TDynamic &&
       not (always_returns body') then
    typeError fi
    "function with non-void return type must always return something"
    else do
      check_returns fi ret_ty body'
      return $ CFunc TVoid static f ret_ty args body'
    else
    typeError fi $ "break or continue outside of loop"

tycheckCommand (CExtends fi ty) = do
  check_ty fi ty
  this_class <- gets s_class
  this_class_static <- gets s_class_static
  parent_class <- get_parent_class this_class
  case parent_class of
    Just c ->
      typeError fi $ "already declared parent class " ++ show parent_class
    Nothing ->
      modify $ \s ->
      s { s_parents =
          Symtab.add (type_name this_class) ty
          (Symtab.add (type_name this_class_static)
           (type_to_static ty) $ s_parents s)}
  return $ CExtends TVoid ty

tycheckCommand (CClass fi cls) = do
  outer_class <- gets s_class
  outer_class_static <- gets s_class_static
  CClass TVoid <$> tycheckClass outer_class outer_class_static cls

tycheckCommand (CSignal fi _ _) =
  typeError fi "signals not supported yet"

tycheckCommand (CClassName fi _ _) =
  typeError fi "'class_name' not supported yet"


tycheckClass :: Show α => Type -> Type -> Class α -> TycheckM (Class Type)
tycheckClass outer_class outer_class_static
  (Class { class_name = nm
         , class_commands = coms }) = do
  let class_ty = TClass outer_class nm
  let class_static_ty = TClassStatic outer_class_static nm
  -- Set the class types in the typechecker state.
  modify $ \s -> s { s_class = class_ty
                   , s_class_static = class_static_ty }
  
  -- Add initial empty namespaces for the current class.
  add_empty_namespace "tycheckClass" class_ty []
  add_empty_namespace "tycheckClass" class_static_ty []
  upd_fun_namespace "tycheckClass" class_static_ty (Id "new") $
    TFunc [(class_ty, [])]

  let extends_coms = filter is_extends_command coms
  let f_coms = filter is_func_command coms
  let phase1_coms = filter is_phase1_command coms
  let phase2_coms = filter is_phase2_command coms
  let f_tys = flip map f_coms $
              \(CFunc _ static f ty args _) ->
                (f, static, TFunc [(ty, snd <$> args)])

  if length (extends_coms) > 1 then
    typeError "tycheckClass" "more than one 'extends' command"
    else return ()
    
  mapM_ tycheckCommand extends_coms
  
  -- Add function bindings to delta and the class namespaces.
  flip mapM_ f_tys $
    \(f, static, ty) -> do
      if static then
        upd_fun_namespace "tycheckClass" class_static_ty f ty
        else
        upd_fun_namespace "tycheckClass" class_ty f ty

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
                       upd_var_namespace "tycheckClass" class_static_ty x ty
                     CEnum _ nm fields -> do
                       let field_binds = (second $ const TInt) <$> fields
                       case nm of
                         Just x -> do
                           -- If the enum is named, add a binding for
                           -- it in the class's static namespace and
                           -- add bindings for its fields in its own
                           -- namespace.
                           let enum_ty = TEnum class_static_ty x
                           upd_var_namespace "tycheckClass" class_static_ty x enum_ty
                           add_empty_namespace "tycheckClass" enum_ty []
                           flip mapM_ field_binds $
                             \(y, ty) ->
                               upd_var_namespace "tycheckClass" enum_ty y ty
                         Nothing ->
                           -- If the enum is not named, add bindings
                           -- for its fields in the class's namespace.
                           flip mapM_ field_binds $
                             \(y, ty) ->
                               upd_var_namespace "tycheckClass" class_ty y ty
                     CClass _ cls@(Class { class_name = inner_class_nm }) -> do
                       let inner_class_ty =
                             TClassStatic class_static_ty inner_class_nm
                       upd_var_namespace "tycheckClass"
                         class_static_ty inner_class_nm inner_class_ty
                   return c'

  phase2_coms' <- flip mapM phase2_coms $
               \c -> do
                 c' <- tycheckCommand c
                 case c' of
                   CVar _ _ _ _ x (Just ty) _ _ ->
                     -- Add binding to class namespace.
                     upd_var_namespace "tycheckClass" class_ty x ty
                 return c'
  
  f_coms' <- flip mapM f_coms $
             \c -> do
               c' <- tycheckCommand c
               case c' of
                 CFunc _ _ f ty args _ ->
                   -- Hmm maybe nothing to do here since the bindings
                   -- have already been added.
                   return ()
               return c'

  -- Restore the original class types in the typechecker state.
  modify $ \s -> s { s_class = outer_class
                   , s_class_static = outer_class_static }

  return $ Class { class_name = nm
                 , class_commands = phase1_coms' ++
                                    phase2_coms' ++
                                    f_coms' }


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


-- Check that a function always returns. Ignore loops and only really
-- care about if conditionals. If all branchs of the conditional
-- return, then we are done. Otherwise, continue searching for a
-- return.
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


-- Check that all return expressions' types are compatible with the
-- argument type.
check_returns :: Show α => α -> Type -> [Stmt Type] -> TycheckM ()
check_returns _ _ [] = return ()
check_returns fi ret_ty (stmt:stmts) = do
  case stmt of
    SIf _ _ if_branch elifs else_branch -> do
      check_returns fi ret_ty if_branch
      mapM_ (secondM $ check_returns fi ret_ty) elifs
      mapM_ (check_returns fi ret_ty) else_branch
    SWhile _ _ body -> check_returns fi ret_ty body
    SFor _ _ _ body -> check_returns fi ret_ty body
    SMatch _ _ cases ->
      mapM_ (secondM $ check_returns fi ret_ty) cases
    SReturn _ e ->
      if data_of_expr e `compatible` ret_ty then
        -- debugPrintM_ $ show (data_of_expr e) ++
        -- "is compatible with " ++ show ret_ty
        return ()
      else
        -- typeError fi $ "type of expression in return statement not \
        --                \compatible with return type of the function. "
        typeError fi $ "returning expression with incompatible type. "
        ++ show (data_of_expr e) ++ " not compatible with " ++ show ret_ty
    _ -> return ()
  check_returns fi ret_ty stmts


tycheck_args :: Show α => α -> Expr α -> [Expr Type] -> [(Type, [Type])] ->
                TycheckM Type
tycheck_args fi f args [] =
  typeError fi $ "no type signature found for function " ++ show f

-- Special case for when there's only a single type signature.
tycheck_args fi f args [(ret_ty, arg_tys)] =
  if length args == length arg_tys then do
    good <- and <$>
      (flip mapM (zip args arg_tys) $
       \(arg, expected) -> do
         let actual = data_of_expr arg
         return $ compatible actual expected)
        -- if compatible actual expected then return ()
        --   else
          -- typeError (data_of_expr arg) $
          -- "incompatible argument type. expected: " ++
          -- show expected ++ ", actual: " ++ show actual
    return $ if good then ret_ty else TDynamic
  else
    return TDynamic
    -- typeError fi $ "wrong number of arguments to function " ++ show f
    -- ++ ". expected: " ++ show (length arg_tys) ++ ", actual: "
    -- ++ show (length args)

-- Look for a compatible type signature.
tycheck_args fi f args overloads = do
  ret_ty <- foldM
    (\acc (ret_ty, arg_tys) ->
       case acc of
         Just _ -> return acc
         Nothing ->
           if length args == length arg_tys then do
             good <- and <$>
               (flip mapM (zip args arg_tys) $
                \(arg, expected) -> do
                  let actual = data_of_expr arg
                  return $ compatible actual expected)
             return $ if good then Just ret_ty else Nothing
           else
             return Nothing)
    Nothing overloads
  case ret_ty of
    Just ty -> return ty
    Nothing ->
      return TDynamic
      -- typeError fi $
      -- "no compatible type signature found for function " ++ show f


---------------------------
-- Typechecker entry point.

-- TODO: may need special type for 'self' containing all static and
-- member things in its namespace.

tycheckMain :: Show α => Class α -> Maybe Value ->
               Either String (Class Type, [String])
tycheckMain cls@(Class { class_name = nm }) api_value =
  let (api_global_namespace,
       api_class_namespaces,
       api_inheritance_map) = case api_value of
                                Just api -> parse_api_namespaces api
                                Nothing  -> (Symtab.empty,
                                             Symtab.empty,
                                             Symtab.empty)
      class_static = TClassStatic TVoid nm
      init_state =
        TycheckState { s_class         = TClass TVoid nm
                     , s_class_static  = class_static
                     , s_constants     = Symtab.empty
                     , s_parents       = api_inheritance_map
                     , s_namespaces    = Symtab.union builtin_namespaces
                                         api_class_namespaces }
      init_ctx = upd_gamma (Symtab.add (Id "self") (TClass TVoid nm)) $
                 mkCtx (Symtab.union builtin_constants api_global_namespace)
                 builtin_function_env
  in
    -- debugPrint (show api_global_namespace) $
    -- debugPrint (show api_class_namespaces) $
    -- debugPrint (show api_inheritance_map) $
    runTycheck init_state init_ctx $ tycheckClass TVoid TVoid cls
