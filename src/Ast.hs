module Ast where

import Data.List (intercalate)

import Symtab (Id)


data Unop =
  UBitwiseNot
  | UNeg
  | UNot
  | UGetNode
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
  deriving Eq

instance Show (Literal α) where
  show (LBool b) = "(LBool " ++ show b ++ ")"
  show (LInt i) = "(LInt " ++ show i ++ ")"
  show (LFloat f) = "(LFloat " ++ show f ++ ")"
  show (LString s) = "(LString " ++ show s ++ ")"
  show (LArray es) = "(LArray (" ++ intercalate " " (show <$> es) ++ "))"
  show (LDict entries) =
    "(LDict (" ++ intercalate " " (show <$> entries) ++ "))"

data Expr α =
  ELiteral α (Literal α)
  | EIdent α Id
  | ECall α (Expr α) [Expr α]
  | EUnop α Unop (Expr α)
  | EBinop α Binop (Expr α) (Expr α)
  | EIfElse α (Expr α) (Expr α) (Expr α)
  deriving Eq

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

data Type =
  TNull
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
  | TObject
  | TArray Type
  | TDictionary Type Type
  | TName Id
  | TVar Id
  | TIntersect [Type]
  deriving (Eq, Show)

data Command α = 
  CEnum α [(Id, Maybe (Expr α))]
  -- export, export list, onready, name, initial value, setget
  | CVar α Bool (Maybe (Type, [Expr α])) Bool Id (Maybe (Expr α))
    (Maybe (Id, Id))
  | CConst α Id (Expr α)
  -- static, name, return type, args, body
  | CFunc α Bool Id Type [(Id, Type)] [Stmt α]
  | CExtends α (Expr α)
  | CClass α (Class α)
  -- | CExport α (Maybe (Type, [Expr α])) Id (Maybe (Expr α))
  -- name, args
  | CSignal α Id [Id]
  deriving Eq

instance Show (Command α) where
  show (CEnum _ entries) = "(CEnum (" ++
    intercalate " "
    ((\(x, e) -> "(" ++ show x ++ " " ++ show e ++ ")") <$> entries)
    ++ "))"
  show (CVar _ export export_list onready name init_val setget) =
    "(CVar " ++ show export ++ " (" ++
    show ((\(t, es) ->
             "(" ++ show t ++ ", (" ++
             intercalate " " (show <$> es) ++ "))")
           <$> export_list)
    ++ ") " ++ show onready ++ " " ++ show name ++ " " ++
    show init_val ++ " " ++ show setget ++ ")"
  show (CConst _ x e) = "(CConst " ++ show x ++ " " ++ show e ++ ")"
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

data Stmt α =
  SPass α
  | SVar α Id (Maybe (Expr α))
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
  deriving Eq

showMaybe :: (a -> String) -> Maybe a -> String
showMaybe _ Nothing = "Nothing"
showMaybe f (Just x) = f x

instance Show (Stmt α) where
  show (SPass _) = "(SPass)"
  show (SVar _ x e) = "(SVar " ++ show x ++ " " ++ show e ++ ")"
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

data Pattern α =
  PLiteral (Literal α)
  | PIdent Id -- not a binder. checks for equality with the given ident
  | PVar Id -- marked by 'var' keyword in concrete syntax
  | PWildcard
  | PArray [Pattern α]
  | PDict [DictEntryPattern α]
  | PMulti [Pattern α]
  deriving (Eq, Show)

data DictEntryPattern α =
  DEPOpen -- '..'
  | DEPKV (Pattern α) (Pattern α) -- key, value
  deriving (Eq, Show)

data Class α = Class { class_name :: Id
                     , class_commands :: [Command α] }
  deriving Eq

instance Show (Class α) where
  show (Class { class_name = nm
              , class_commands = coms }) =
    "(Class " ++ show nm ++ " (" ++ intercalate " " (show <$> coms) ++ "))"
