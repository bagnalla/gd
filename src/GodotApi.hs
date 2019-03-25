{-# LANGUAGE OverloadedStrings #-}

module GodotApi where

-- Ignoring signals for now.
-- Also ignore getters and setters.

import Control.Monad.State
import Data.Aeson
import Data.Bifunctor (first)
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as H
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as V

import Ast
import Namespace
import Symtab

-- Assume all enums are named.

type NamespaceRaw = (String, -- name
                     String, -- base class
                     [(String, Type)], -- constants
                     [(String, Type)], -- properties
                     [(String, Type)], -- methods
                     [(String, [String])]) -- enums


-- Returns global namespace, class namespaces, inheritance map
parse_api_namespaces :: Value -> (Symtab Type, Symtab Namespace, Symtab Type)
parse_api_namespaces (Array arr) =
  let globs = parse_global_constants $ object_of $ V.head arr
      namespaces = parse_namespaces $ V.tail arr
  in
    (build_global_namespace globs,
     build_namespaces namespaces,
     build_inheritance_map namespaces)

-- Assume all global constants have type int.
build_global_namespace :: [String] -> Symtab Type
build_global_namespace names =
  Symtab.fromList $ (\nm -> (Id nm, TInt)) <$> names

build_class_namespaces :: [NamespaceRaw] -> Symtab Namespace
build_class_namespaces raws =
  Symtab.fromList $
  (\(nm, _, consts, props, methods, _) ->
     let vars = Symtab.fromList $ (first Id) <$> consts ++ props
         funs = Symtab.fromList $ (first Id) <$> methods
     in
       (Id nm, mkNamespace [] vars funs))
  <$> raws

build_enum_namespaces :: [NamespaceRaw] -> Symtab Namespace
build_enum_namespaces raws =
  Symtab.fromList $ concat $ go <$> raws
  where
    go :: NamespaceRaw -> [(Id, Namespace)]
    go (class_nm, _, _, _, _, enums) =
      enum_namespace class_nm <$> enums
    enum_namespace :: String -> (String, [String]) -> (Id, Namespace)
    enum_namespace class_nm (enum_nm, field_nms) =
      let enum_ty = TEnum (TClass TVoid $ Id class_nm) $ Id enum_nm
          enum_vars = Symtab.fromList $ (\nm -> (Id nm, TInt)) <$> field_nms
      in
        (type_name enum_ty, mkNamespace [] enum_vars Symtab.empty)

build_namespaces :: [NamespaceRaw] -> Symtab Namespace
build_namespaces raws =
  Symtab.union (build_class_namespaces raws) (build_enum_namespaces raws)

parse_global_constants :: Object -> [String]
parse_global_constants o =
  T.unpack <$> H.keys (object_of $ fromJust $ H.lookup "constants" o)

build_inheritance_map :: [NamespaceRaw] -> Symtab Type
build_inheritance_map raws =
  Symtab.fromList $
  (\(class_nm, base_class_nm, _, _, _, _) ->
       (Id class_nm, TClass TVoid $ Id base_class_nm))
  <$> raws

-- parse_namespaces :: V.Vector Value
parse_namespaces :: V.Vector Value -> [NamespaceRaw]
parse_namespaces arr = V.toList $ parse_namespace . object_of <$> arr

parse_namespace :: Object -> NamespaceRaw
parse_namespace o =
  let name       = string_of $ fromJust $ H.lookup "name" o
      base_class = string_of $ fromJust $ H.lookup "base_class" o
      constants  = parse_constants $ object_of $
                   fromJust $ H.lookup "constants" o
      properties = V.toList $ parse_property . object_of <$>
                   (array_of $ fromJust $ H.lookup "properties" o)
      methods    = V.toList $ parse_method . object_of <$>
                   (array_of $ fromJust $ H.lookup "methods" o)
      enums      = V.toList $ parse_enum . object_of <$>
                   (array_of $ fromJust $ H.lookup "enums" o)
  in
    (name, base_class, constants, properties, methods, enums)

parse_constants :: Object -> [(String, Type)]
parse_constants o = (\k -> (T.unpack k, TInt)) <$> H.keys o

parse_property :: Object -> (String, Type)
parse_property o =
  let name = string_of $ fromJust $ H.lookup "name" o
      ty   = parse_type $ string_of $ fromJust $ H.lookup "type" o
  in
    (name, ty)

parse_method :: Object -> (String, Type)
parse_method o =
  let name        = string_of $ fromJust $ H.lookup "name" o
      return_type = parse_type $ string_of $
                    fromJust $ H.lookup "return_type" o
      arguments   = V.toList $ array_of $
                    fromJust $ H.lookup "arguments" o
      arg_lists   = go $ parse_argument <$> object_of <$> arguments
      signatures = (\args -> (return_type, args)) <$> arg_lists
  in
    (name, TFunc signatures)
  where
    -- Deal with overloads.
    go :: [(Type, Bool)] -> [[Type]]
    go [] = [[]]
    go args = (fst <$> args) :
              (if snd (last args) then go (init args) else [])

parse_argument :: Object -> (Type, Bool)
parse_argument o =
  let ty = parse_type $ string_of $ fromJust $ H.lookup "type" o
      has_default = bool_of $ fromJust $ H.lookup "has_default_value" o
  in
    (ty, has_default)

parse_enum :: Object -> (String, [String])
parse_enum o =
  let name = string_of $ fromJust $ H.lookup "name" o
      values = parse_enum_values $ object_of $
               fromJust $ H.lookup "values" o
  in
    (name, values)

parse_enum_values :: Object -> [String]
parse_enum_values o = T.unpack <$> H.keys o


object_of :: Value -> Object
object_of (Object o) = o
object_of _ = error "expected object"

array_of :: Value -> V.Vector Value
array_of (Array a) = a
array_of _ = error "expected array"

string_of :: Value -> String
string_of (String s) = T.unpack s
string_of _ = error "expected string"

number_of :: Value -> Scientific
number_of (Number n) = n
number_of _ = error "expected number"

bool_of :: Value -> Bool
bool_of (Bool b) = b
bool_of _ = error "expected bool"

null_of :: Value -> ()
null_of Null = ()
null_of _ = error "expected null"


parse_type :: String -> Type
parse_type "void" = TVoid
parse_type "null" = TNull
parse_type "int" = TInt
parse_type "bool" = TBool
parse_type "float" = TFloat
parse_type "String" = TString
parse_type "Vector2" = TVector2
parse_type "Rect2" = TRect2
parse_type "Vector3" = TVector3
parse_type "Transform2D" = TTransform2D
parse_type "Plane" = TPlane
parse_type "Quat" = TQuat
parse_type "AABB" = TAABB
parse_type "Basis" = TBasis
parse_type "Transform" = TTransform
parse_type "Color" = TColor
parse_type "NodePath" = TNodePath
parse_type "RID" = TRID
parse_type "PoolColorArray" = TPoolColorArray
parse_type "PoolVector3Array" = TPoolVector3Array
parse_type "PoolVector2Array" = TPoolVector2Array
parse_type "PoolStringArray" = TPoolStringArray
parse_type "PoolRealArray" = TPoolRealArray
parse_type "PoolIntArray" = TPoolIntArray
parse_type "PoolByteArray" = TPoolByteArray
parse_type "Array" = TArray TDynamic
parse_type "Dictionary" = TDict TDynamic TDynamic
parse_type _ = TDynamic
