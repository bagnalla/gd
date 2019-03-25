module TycheckUtil where

import Data.Bifunctor (first)

import Ast
import Intrinsics
import Namespace
import Symtab

string_or_ident :: Expr α -> Bool
string_or_ident (ELiteral _ (LString _)) = True
string_or_ident (EIdent _ _) = True
string_or_ident _ = False

-- Can 's' be used where 't' is expected? This is basically a subtype
-- check.
-- TODO: Take inheritance into account.
compatible :: Type -> Type -> Bool
compatible s TDynamic = True
compatible TInt TFloat = True
compatible (TArray s) (TArray t) = compatible s t
compatible (TDict k1 v1) (TDict k2 v2) =
  compatible k1 k2 && compatible v1 v2
-- compatible (TClass outer_x x) (TClass outer_y y) = subtype check
compatible (TClassStatic _ _) _ = error "static class in 'compatible' check"
compatible _ (TClassStatic _ _) = error "static class in 'compatible' check"
compatible s t = s == t

is_numeric :: Type -> Bool
is_numeric TInt = True
is_numeric TFloat = True
is_numeric _ = False

is_vector :: Type -> Bool
is_vector TVector2 = True
is_vector TVector3 = True
is_vector _ = False

numeric_result :: Type -> Type -> Type
numeric_result TInt TInt = TInt
numeric_result TInt TFloat = TFloat
numeric_result TFloat TInt = TFloat
numeric_result TFloat TFloat = TFloat
numeric_result _ _ = TDynamic

-- Are 's + t' and 's - t' legal?
can_be_added :: Type -> Type -> Bool
can_be_added TDynamic TDynamic = True
can_be_added s t =
  is_numeric s && is_numeric t ||
  is_vector s && is_vector t

-- -- Are 's + t' and 's - t' legal?
-- can_be_added :: Type -> Type -> Bool
-- can_be_added TInt TInt = True
-- can_be_added TInt TFloat = True
-- can_be_added TFloat TInt = True
-- can_be_added TFloat TFloat = True
-- can_be_added TVector2 TVector2 = True
-- can_be_added TVector3 TVector3 = True
-- can_be_added TDynamic TDynamic = True
-- can_be_added s t = False

-- Is 's * t' legal?
can_be_multiplied :: Type -> Type -> Bool
can_be_multiplied TDynamic TDynamic = True
can_be_multiplied s t =
  is_numeric s && (is_numeric t || is_vector t) ||
  is_numeric t && (is_numeric s || is_vector s)

-- -- Is 's * t' legal?
-- can_be_multiplied :: Type -> Type -> Bool
-- can_be_multiplied TInt TInt = True
-- can_be_multiplied TInt TFloat = True
-- can_be_multiplied TFloat TInt = True
-- can_be_multiplied TFloat TFloat = True
-- can_be_multiplied TInt TVector2 = True
-- can_be_multiplied TInt TVector3 = True
-- can_be_multiplied TVector2 TInt = True
-- can_be_multiplied TVector3 TInt = True
-- can_be_multiplied TFloat TVector2 = True
-- can_be_multiplied TFloat TVector3 = True
-- can_be_multiplied TVector2 TFloat = True
-- can_be_multiplied TVector3 TFloat = True
-- can_be_multiplied TDynamic TDynamic = True
-- can_be_multiplied s t = False

-- Is 's / t' legal?
can_be_divided :: Type -> Type -> Bool
can_be_divided TDynamic TDynamic = True
can_be_divided s t =
  (is_numeric s || is_vector s) && is_numeric t

-- -- Is 's / t' legal?
-- can_be_divided :: Type -> Type -> Bool
-- can_be_divided TInt TInt = True
-- can_be_divided TInt TFloat = True
-- can_be_divided TFloat TInt = True
-- can_be_divided TFloat TFloat = True
-- can_be_divided TVector2 TInt = True
-- can_be_divided TVector2 TFloat = True
-- can_be_divided TDynamic TDynamic = True
-- can_be_divided s t = False

numeric_binop :: Binop -> Bool
numeric_binop BPlus  = True
numeric_binop BMinus = True
numeric_binop BMult  = True
numeric_binop BDiv   = True
numeric_binop BPlusAssign  = True
numeric_binop BMinusAssign = True
numeric_binop BMultAssign  = True
numeric_binop BDivAssign   = True
numeric_binop _ = False

assign_binop :: Binop -> Bool
assign_binop BAssign      = True
assign_binop BPlusAssign  = True
assign_binop BMinusAssign = True
assign_binop BMultAssign  = True
assign_binop BDivAssign   = True
assign_binop BModAssign   = True
assign_binop BAndAssign   = True
assign_binop BOrAssign    = True
assign_binop _ = False

int_binop :: Binop -> Bool
int_binop BMod = True
int_binop BShiftL = True
int_binop BShiftR = True
int_binop BBitwiseAnd = True
int_binop BBitwiseXor = True
int_binop BBitwiseOr = True
int_binop BModAssign = True
int_binop _ = False

-- Binops for which the operands must be the same type (or both
-- numeric).
same_type_binop :: Binop -> Bool
same_type_binop BLt = True
same_type_binop BLe = True
same_type_binop BGt = True
same_type_binop BGe = True
same_type_binop BEq = True
same_type_binop BNeq = True
same_type_binop _ = False

bool_binop :: Binop -> Bool
bool_binop BAnd = True
bool_binop BOr = True
bool_binop BAndAssign = True
bool_binop BOrAssign = True
bool_binop _ = False

join_types :: Type -> Type -> Type
join_types TInt TFloat = TFloat
join_types TFloat TInt = TFloat
join_types TDynamic _ = TDynamic
join_types _ TDynamic = TDynamic
join_types s t =
  if s == t then s else error "join_types: incompatible types"

-- BPlus   number, number
-- BMinus  number, number
-- BMult   number, number
-- BDiv    number, number
-- BIs     anything, type
-- BMod    int, int
-- BShiftL int, int
-- BShiftR int, int
-- BBitwiseAnd int, int
-- BBitwiseXor int, int
-- BBitwiseOr int, int
-- BLt same types
-- BLe same types
-- BGt same types
-- BGe same types
-- BEq same types
-- BNeq same types
-- BIn anything, container of that type (keys for dict)
-- BAnd anything at all for either (maybe we want to restrict this to bools)
-- BOr anything at all for either (maybe we want to restrict this to bools)
-- BIndex array or dict
-- BAttribute anything, identifier of any valid attribute for the left type
-- BAssign anything, anything that matches the left
-- BPlusAssign number, number
-- BMinusAssign number, number
-- BMultAssign number, number
-- BDivAssign number, number
-- BModAssign int, int
-- BAndAssign anything, anything (maybe restrict to bool)
-- BOrAssign anything, anything (maybe restrict to bool)

is_func_command :: Command α -> Bool
is_func_command (CFunc _ _ _ _ _ _) = True
is_func_command _ = False

is_const_command :: Command α -> Bool
is_const_command (CConst _ _ _ _) = True
is_const_command _ = False

is_var_command :: Command α -> Bool
is_var_command (CVar _ _ _ _ _ _ _ _) = True
is_var_command _ = False

is_extends_command :: Command α -> Bool
is_extends_command (CExtends _ _) = True
is_extends_command _ = False

is_break_statement :: Stmt α -> Bool
is_break_statement (SBreak _) = True
is_break_statement _ = False

is_continue_statement :: Stmt α -> Bool
is_continue_statement (SContinue _) = True
is_continue_statement _ = False

is_literal_pattern :: Pattern α -> Bool
is_literal_pattern (PLiteral _) = True
is_literal_pattern _ = False

is_open_entry_pattern :: DictEntryPattern α -> Bool
is_open_entry_pattern DEPOpen = True
is_open_entry_pattern _ = False

is_phase1_command :: Command α -> Bool
is_phase1_command (CConst _ _ _ _) = True
is_phase1_command (CEnum _ _ _) = True
is_phase1_command (CClass _ _) = True
is_phase1_command _ = False

is_phase2_command :: Command α -> Bool
is_phase2_command (CVar _ _ _ _ _ _ _ _) = True
is_phase2_command _ = False


-- Checking if expressions are considered constant.
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
      x `elem` (fst <$> constant_functions)
    _ -> False

-- isConstPattern :: Symtab (Expr Type) -> Pattern α -> Bool
-- isConstPattern γ (PLiteral lit) = isConstLiteral γ lit
-- isConstPattern γ (PIdent _) = True
-- isConstPattern γ (PVar _) = False


-- This should suffice I think.
no_breaks_continues :: [Stmt α] -> Bool
no_breaks_continues stmts =
  not (any is_break_statement stmts || any is_continue_statement stmts)


-- Built-in namespaces / global environment.

builtin_namespaces :: Symtab Namespace
builtin_namespaces = fromList $ go <$> builtin_type_namespaces
  where
    go :: (Id, [String], [(String, Type)], [(String, Type)]) ->
          (Id, Namespace)
    go (x, ty_params, var_bindings, fun_bindings) =
      (x, Namespace { ns_params = Id <$> ty_params
                    , ns_vars = fromList $ first Id <$> var_bindings
                    , ns_funs = fromList $ first Id <$> fun_bindings})

builtin_function_env :: Symtab Type
builtin_function_env =
  fromList $ first Id <$> (builtin_constructors ++
                           builtin_functions ++
                           constant_functions)

builtin_constants :: Symtab Type
builtin_constants = fromList $ first Id <$> constant_vars
