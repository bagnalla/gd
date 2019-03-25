module Namespace where

import Ast
import Intrinsics
import Symtab (add, empty, get, Id(..), map, Symtab)


-- A namespace can be generic (have type parameters). It can be
-- instantiated to a concrete namespace by substituting types for all
-- of the params.
data Namespace = Namespace { ns_params    :: [Id]
                           , ns_vars :: Symtab Type
                           , ns_funs :: Symtab Type }
  deriving Show

new :: [Id] -> Namespace
new params = Namespace { ns_params = params
                       , ns_vars = empty
                       , ns_funs = empty }

mkNamespace :: [Id] -> Symtab Type -> Symtab Type -> Namespace
mkNamespace params vars funs =
  Namespace { ns_params = params
            , ns_vars = vars
            , ns_funs = funs }

add_var :: Id -> Type -> Namespace -> Namespace
add_var x ty ns@(Namespace { ns_vars = vars }) =
  ns { ns_vars = Symtab.add x ty vars }

add_fun :: Id -> Type -> Namespace -> Namespace
add_fun x ty ns@(Namespace { ns_funs = funs }) =
  ns { ns_funs = Symtab.add x ty funs }

-- get :: Id -> Namespace -> Maybe Type
-- get x (Namespace { ns_namespace = ns }) = Symtab.get x ns

open_vars :: Namespace -> Symtab Type
open_vars = ns_vars

open_funs :: Namespace -> Symtab Type
open_funs = ns_funs

-- Substitute s for t in a namespace symbol table.
subst_namespace :: Type -> Type -> Symtab Type -> Symtab Type
subst_namespace s t = Symtab.map $ \x -> if x == t then s else x

-- Instantiate a namespace 
-- instantiate_namespace :: Type -> Namespace -> Maybe (Symtab Type)
-- instantiate_namespace (TArray ty) (Namespace { ns_params = [ty_arg]
--                                              , ns_namespace = ns }) =
--   Just $ subst_namespace ty (TVar ty_arg) ns
-- instantiate_namespace (TArray _) _ = Nothing
-- instantiate_namespace (TDict k v) (Namespace { ns_params = [k_arg, v_arg]
--                                              , ns_namespace = ns }) =
--   Just $ subst_namespace v (TVar v_arg) $
--   subst_namespace k (TVar k_arg) ns
-- instantiate_namespace (TDict _ _) _ = Nothing
-- instantiate_namespace _ (Namespace { ns_params = []
--                                    , ns_namespace = ns }) =
--   Just ns
-- instantiate_namespace _ _ = Nothing

instantiate_namespace :: [Type] -> Namespace ->
                         Maybe (Symtab Type, Symtab Type)
instantiate_namespace ty_args (Namespace { ns_params = ty_params
                                         , ns_vars = vars
                                         , ns_funs = funs }) =
  go ty_args (TVar <$> ty_params) vars funs
  where
    go :: [Type] -> [Type] -> Symtab Type -> Symtab Type ->
          Maybe (Symtab Type, Symtab Type)
    go (arg:args) (param:params) vars funs =
      go args params
      (subst_namespace arg param vars)
      (subst_namespace arg param funs)
    go [] [] vars funs = Just (vars, funs)
    go _ _ _ _ = Nothing
