module SemanticsUtils where

import Utils
import qualified TypedAST
import TypedAST (StaticInit (..), intStaticInit)
import AST
import ParserUtils (typeSize)

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

exprToBool :: Expr -> Maybe Bool
exprToBool expr = evalConst expr >>= (\x -> if x /= 0 then pure True else pure False)

-- evaluate const expression, return Nothing for non-const
isInitConst :: Type_ -> VarInit -> Maybe [StaticInit]
isInitConst type_ init_ = case init_ of
  SingleInit expr -> intStaticInit type_ <$> evalConst expr
  CompoundInit inits -> case type_ of
    ArrayType inner _ ->
      take (typeSize type_) . (++ repeat (IntInit 0)) . concatMap (take (typeSize inner) . (++ repeat (IntInit 0))) <$> traverse (isInitConst inner) inits
    _ -> Nothing

zeroInitializer :: Type_ -> TypedAST.VarInit
zeroInitializer type_@(ArrayType inner size) =
  TypedAST.CompoundInit (replicate size (zeroInitializer inner)) type_
zeroInitializer type_ = TypedAST.SingleInit (TypedAST.litExpr 0 type_) type_

-- evaluate const expression, return Nothing for non-const
evalConst :: Expr -> Maybe Int
evalConst expr = case expr of
  Lit (ConstInt n) -> pure n
  Lit (ConstUInt n) -> pure n
  Lit (ConstLong n) -> pure n
  Lit (ConstULong n) -> pure n
  Unary Complement f -> complement <$> evalConst f
  Unary Negate f -> (* (- 1)) <$> evalConst f
  Unary BoolNot f -> (\x -> if x == 0 then 1 else 0) <$> evalConst f
  Var _ -> Nothing
  FunctionCall _ _ -> Nothing
  Cast _ expr' -> evalConst expr' -- TODO: actually cast stuff here
  Binary SubOp left right -> liftA2 (-) (evalConst left) (evalConst right)
  Binary AddOp left right -> liftA2 (+) (evalConst left) (evalConst right)
  Binary MulOp left right -> liftA2 (*) (evalConst left) (evalConst right)
  Binary DivOp left right -> liftA2 div (evalConst left) (evalConst right)
  Binary ModOp left right -> liftA2 mod (evalConst left) (evalConst right)
  Binary BitAnd left right -> liftA2 (.&.) (evalConst left) (evalConst right)
  Binary BitOr left right -> liftA2 (.|.) (evalConst left) (evalConst right)
  Binary BitXor left right -> liftA2 xor (evalConst left) (evalConst right)
  Binary BitShr left right -> liftA2 shift (evalConst left) ((* (-1)) <$> evalConst right)
  Binary BitShl left right -> liftA2 shift (evalConst left) (evalConst right)
  Binary BoolAnd left right -> boolToInt <$> liftA2 (&&) (exprToBool left) (exprToBool right)
  Binary BoolOr left right -> boolToInt <$> liftA2 (||) (exprToBool left) (exprToBool right)
  Binary BoolEq left right -> boolToInt <$> liftA2 (==) (exprToBool left) (exprToBool right)
  Binary BoolLe left right -> boolToInt <$> liftA2 (<) (exprToBool left) (exprToBool right)
  Binary BoolGe left right -> boolToInt <$> liftA2 (>) (exprToBool left) (exprToBool right)
  Binary BoolLeq left right -> boolToInt <$> liftA2 (<=) (exprToBool left) (exprToBool right)
  Binary BoolGeq left right -> boolToInt <$> liftA2 (>=) (exprToBool left) (exprToBool right)
  Binary {} -> Nothing
  Assign {} -> Nothing
  PostAssign {} -> Nothing
  Conditional c trueExpr falseExpr ->
    evalConst c >>= (\x -> if x /= 0 then evalConst trueExpr else evalConst falseExpr)
  AddrOf {} -> Nothing
  Dereference {} -> Nothing
  Subscript {} -> Nothing