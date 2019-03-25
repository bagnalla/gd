{-# LANGUAGE DeriveFunctor #-}

module Ast where

import Data.List (intercalate)

import Symtab (Id(..))


data Unop =
  UBitwiseNot
  | UNeg
  | UNot
  | UGetNode
  | UAttribute -- for weird 'super' syntax
  deriving (Eq, Show)

data Binop =
  BPlus
  | BMinus
  | BMult
  | BDiv
  | BIs
  | BMod
  | BShiftL
  | BShiftR
  | BBitwiseAnd
  | BBitwiseXor
  | BBitwiseOr
  | BLt
  | BLe
  | BGt
  | BGe
  | BEq
  | BNeq
  | BIn
  | BAnd
  | BOr
  | BIndex
  | BAttribute
  | BAssign
  | BPlusAssign
  | BMinusAssign
  | BMultAssign
  | BDivAssign
  | BModAssign
  | BAndAssign
  | BOrAssign
  deriving (Eq, Show)

-- Constructors for built-in types and some built-in functions
-- e.g. 'sin' are considered literals by GDScript (for the sake of
-- appearing in const expressions), but we will parse them as regular
-- function applications and determine during typechecking whether
-- they should be considered literals or not. Even array and
-- dictionary literals here will have to be checked to see if all of
-- their arguments are constant expressions.
data Literal α =
  LBool Bool
  | LInt Integer
  | LFloat Double
  | LString String
  | LArray [Expr α]
  | LDict [(Expr α, Expr α)]
  deriving (Eq, Functor)

data Expr α =
  ELiteral α (Literal α)
  | EIdent α Id
  | ECall α (Expr α) [Expr α]
  | EUnop α Unop (Expr α)
  | EBinop α Binop (Expr α) (Expr α)
  | EIfElse α (Expr α) (Expr α) (Expr α)
  -- | EType α Type
  deriving (Eq, Functor)

data Type =
  TVoid
  | TNull
  | TBool
  | TInt
  | TFloat
  | TString
  | TVector2
  | TRect2
  | TVector3
  | TTransform2D
  | TPlane
  | TQuat
  | TAABB
  | TBasis
  | TTransform
  | TColor
  | TNodePath
  | TRID
  | TPoolColorArray
  | TPoolVector3Array
  | TPoolVector2Array
  | TPoolStringArray
  | TPoolRealArray
  | TPoolIntArray
  | TPoolByteArray
  | TArray Type
  | TDict Type Type
  -- outer class, name
  | TClass Type Id
  | TClassStatic Type Id
  | TEnum Type Id
  | TDynamic
  -- Overloads
  | TFunc [(Type, [Type])]
  | TVar Id
  deriving (Eq, Ord)

-- The primitive built-in types (not including TDynamic).
builtin_types :: [Type]
builtin_types =
  [TVoid, TNull, TBool, TInt, TFloat, TString, TVector2, TRect2, TVector3,
   TTransform2D, TPlane, TQuat, TAABB, TBasis, TTransform, TColor,
   TNodePath, TRID, TPoolColorArray, TPoolVector3Array, TPoolVector2Array,
   TPoolStringArray, TPoolRealArray, TPoolIntArray, TPoolByteArray]
  
type_args :: Type -> [Type]
type_args (TArray ty) = [ty]
type_args (TDict k v) = [k, v]
type_args _ = []


data Command α = 
  CEnum α (Maybe Id) [(Id, Maybe (Expr α))]
  -- export, export list, onready, name, type, initial value, setget
  | CVar α Bool (Maybe (Type, [Expr α])) Bool Id (Maybe Type) (Maybe (Expr α))
    (Maybe (Id, Id))
  | CConst α Id (Maybe Type) (Expr α)
  -- static, name, return type, args, body
  | CFunc α Bool Id Type [(Id, Type)] [Stmt α]
  -- | CExtends α (Expr α)
  | CExtends α Type
  -- | CExtends α Id
  | CClass α (Class α)
  -- name, args
  | CSignal α Id [Id]
  | CClassName α Id (Maybe String)
  deriving (Eq, Functor)

data Stmt α =
  SPass α
  | SVar α Id (Maybe Type) (Maybe (Expr α))
  | SIf α (Expr α) [Stmt α] [(Expr α, [Stmt α])] (Maybe [Stmt α])
  | SWhile α (Expr α) [Stmt α]
  | SFor α Id (Expr α) [Stmt α]
  -- Can have multiple patterns separated by commas (not allowed to be
  -- binding patterns in that case)
  | SMatch α (Expr α) [(Pattern α, [Stmt α])]
  | SBreak α
  | SContinue α
  | SAssert α (Expr α)
  | SReturn α (Expr α)
  | SExpr α (Expr α)
  deriving (Eq, Functor)

data Pattern α =
  PLiteral (Literal α)
  | PIdent Id -- not a binder. checks for equality with the given ident
  | PVar Id -- marked by 'var' keyword in concrete syntax
  | PWildcard
  | PArray [Pattern α]
  | PDict [DictEntryPattern α]
  | PMulti [Pattern α]
  deriving (Eq, Functor, Show)

data DictEntryPattern α =
  DEPOpen -- '..'
  | DEPKV (Pattern α) (Pattern α) -- key, value
  deriving (Eq, Functor, Show)

data Class α = Class { class_name :: Id
                     , class_commands :: [Command α] }
  deriving (Eq, Functor)


-----------------
-- | Misc helpers

data_of_expr :: Expr α -> α
data_of_expr (ELiteral fi _)    = fi
data_of_expr (EIdent fi _)      = fi
data_of_expr (ECall fi _ _)     = fi
data_of_expr (EUnop fi _ _)     = fi
data_of_expr (EBinop fi _ _ _)  = fi
data_of_expr (EIfElse fi _ _ _) = fi


-----------------------------------------------------------------
-- | S-expression Show instances (without printing the extra data
-- e.g. position or type info).

showMaybe :: (a -> String) -> Maybe a -> String
showMaybe _ Nothing = "Nothing"
showMaybe f (Just x) = f x

instance Show (Literal α) where
  show (LBool b) = "(LBool " ++ show b ++ ")"
  show (LInt i) = "(LInt " ++ show i ++ ")"
  show (LFloat f) = "(LFloat " ++ show f ++ ")"
  show (LString s) = "(LString " ++ show s ++ ")"
  show (LArray es) = "(LArray (" ++ intercalate " " (show <$> es) ++ "))"
  show (LDict entries) =
    "(LDict (" ++ intercalate " " (show <$> entries) ++ "))"

instance Show (Expr α) where
  show (ELiteral _ lit) = "(ELiteral " ++ show lit ++ ")"
  show (EIdent _ x) = "(EIdent " ++ show x ++ ")"
  show (ECall _ f args) = "(ECall " ++ show f ++ " " ++ "(" ++
    intercalate ", " (show <$> args) ++ "))"
  show (EUnop _ u e) = "(EUnop " ++ show u ++ " " ++ show e ++ ")"
  show (EBinop _ b e1 e2) = "(EBinop " ++ show b ++ " " ++ show e1
    ++ " " ++ show e2 ++ ")"
  show (EIfElse _ e1 e2 e3) = "(EIfElse " ++ show e1 ++ " "
    ++ show e2 ++ " " ++ show e3 ++ ")"
  -- show (EType _ t) = "(EType " ++ show t ++ ")"

instance Show (Command α) where
  show (CEnum _ nm entries) = "(CEnum " ++ show nm ++ " (" ++
    intercalate " "
    ((\(x, e) -> "(" ++ show x ++ " " ++ show e ++ ")") <$> entries)
    ++ "))"
  show (CVar _ export export_list onready name ty init_val setget) =
    "(CVar " ++ show export ++ " (" ++
    show ((\(t, es) ->
             "(" ++ show t ++ ", (" ++
             intercalate " " (show <$> es) ++ "))")
           <$> export_list)
    ++ ") " ++ show onready ++ " " ++ show name ++ " " ++
    showMaybe show ty ++ " " ++ show init_val ++ " " ++ show setget ++ ")"
  show (CConst _ x t e) =
    "(CConst " ++ show x ++ " " ++ show t ++ " " ++ show e ++ ")"
  show (CFunc _ static name ret_ty args body) =
    "(CFunc " ++ show static ++ " " ++ show name ++ " "
    ++ show ret_ty ++ " (" ++
    intercalate " " ((\(x, t) ->
                         "(" ++ show x ++ " " ++ show t ++ ")") <$> args)
    ++ ") (" ++ intercalate " " (show <$> body) ++ "))"
  show (CExtends _ e) = "(CExtends " ++ show e ++ ")"
  show (CClass _ cls) = "(CClass " ++ show cls ++ ")"
  show (CSignal _ f args) = "(CSignal " ++ show f ++ " (" ++
    intercalate " " (show <$> args) ++ "))"
  show (CClassName _ nm path) =
    "(CClassName " ++ show nm ++ " " ++ showMaybe show path ++ ")"

instance Show (Stmt α) where
  show (SPass _) = "(SPass)"
  show (SVar _ x ty e) =
    "(SVar " ++ show x ++ " " ++ showMaybe show ty ++ " " ++ show e ++ ")"
  show (SIf _ e if_body elifs els) = "(SIf " ++ show e ++ " (" ++
    intercalate " " (show <$> if_body) ++ ") (" ++
    intercalate " " ((\(e', ss) ->
                         "(" ++ show e ++ " (" ++
                         intercalate " " (show <$> ss) ++ "))")
                      <$> elifs) ++ ") (" ++
    (showMaybe (\els' ->
                  "(" ++ intercalate " " (show <$> els') ++ ")")
      els) ++ "))"
  show (SWhile _ e body) = "(SWhile " ++ show e ++ " (" ++
    intercalate " " (show <$> body) ++ "))"
  show (SFor _ x e body) =
    "(SFor " ++ show x ++ " " ++ show e ++ " (" ++
    intercalate " " (show <$> body) ++ "))"
  show (SMatch _ e cases) =
    "(SMatch " ++ show e ++ "( " ++
    intercalate " " ((\(p, ss) ->
                        "(" ++ show p ++ "(" ++
                        intercalate "" (show <$> ss) ++ "))")
                      <$> cases) ++ "))"
  show (SBreak _) = "(SBreak)"
  show (SContinue _) = "(SContinue)"
  show (SAssert _ e) = "(SAssert " ++ show e ++ ")"
  show (SReturn _ e) = "(SReturn " ++ show e ++ ")"
  show (SExpr _ e) = "(SExpr " ++ show e ++ ")"

instance Show (Class α) where
  show (Class { class_name = nm
              , class_commands = coms }) =
    "(Class " ++ show nm ++ " (" ++
    intercalate " " (show <$> coms) ++ "))"

-- TODO: fill missing types
-- Static types are postfixed by "@static" so they differ from the
-- regular names in a way that can't appear in a legal identifier.
instance Show Type where
  show TVoid                  = "void"
  show TNull                  = "null"
  show TBool                  = "bool"
  show TInt                   = "int"
  show TFloat                 = "float"
  show TString                = "String"
  show TVector2               = "Vector2"
  show TRect2                 = "Rect2"
  show TVector3               = "Vector3"
  show TTransform2D           = "Transform2D"
  show TPlane                 = "Plane"
  show TQuat                  = "Quat"
  show TAABB                  = "AABB"
  show TBasis                 = "Basis"
  show TTransform             = "Transform"
  show TColor                 = "Color"
  show TNodePath              = "NodePath"
  show TRID                   = "RID"
  show TPoolColorArray        = "TPoolColorArray"
  show TPoolVector3Array      = "TPoolVector3Array"
  show TPoolVector2Array      = "TPoolVector2Array"
  show TPoolStringArray       = "TPoolStringArray"
  show TPoolRealArray         = "TPoolRealArray"
  show TPoolIntArray          = "TPoolIntArray"
  show TPoolByteArray         = "TPoolByteArray"
  show (TArray ty)            = "(array " ++ show ty ++ ")"
  show (TDict k v)            = "(dict " ++ show k ++ " " ++ show v ++ ")"
  show (TClass TVoid x)       = show x
  show (TClass ty x)          = show ty ++ "." ++ show x
  show (TClassStatic TVoid x) = show x ++ "@static"
  show (TClassStatic ty x)    = show ty ++ "." ++ show x ++ "@static"
  show (TEnum TVoid x)        = show x
  show (TEnum ty x)           = show ty ++ "." ++ show x
  show TDynamic               = "dynamic"
  show (TFunc sigs)           = "(function: " ++ show sigs ++ ")"
  show (TVar x)               = "(TVar " ++ show x ++ ")"

-- Compute a fully qualified lookup name for a type.
type_name :: Type -> Id
-- type_name = Id . show
type_name (TArray _) = Id "@array"
type_name (TDict _ _) = Id "@dict"
type_name ty = Id $ show ty

type_to_static :: Type -> Type
type_to_static (TClass par x) = TClassStatic (type_to_static par) x
type_to_static ty = ty
