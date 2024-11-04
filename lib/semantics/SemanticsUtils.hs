module SemanticsUtils where

import Utils
import AST

isFactorConst :: Factor -> Maybe Int
isFactorConst factor = case factor of
  Lit (ConstInt n) -> pure n
  Lit (ConstUInt n) -> pure n
  Unary Complement f -> complement <$> isFactorConst f
  Unary Negate f -> (* (- 1)) <$> isFactorConst f
  Unary BoolNot f -> (\x -> if x == 0 then 1 else 0) <$> isFactorConst f
  FactorExpr expr -> isConst expr
  Var _ -> Nothing
  FunctionCall _ _ -> Nothing
  Cast _ expr -> isConst expr -- TODO: actually cast stuff here

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

exprToBool :: Expr -> Maybe Bool
exprToBool expr = isConst expr >>= (\x -> if x /= 0 then pure True else pure False)

-- evaluate const expression, return Nothing for non-const
isConst :: Expr -> Maybe Int
isConst expr = case expr of
  Factor f -> isFactorConst f
  Binary SubOp left right -> liftA2 (-) (isConst left) (isConst right)
  Binary AddOp left right -> liftA2 (+) (isConst left) (isConst right)
  Binary MulOp left right -> liftA2 (*) (isConst left) (isConst right)
  Binary DivOp left right -> liftA2 div (isConst left) (isConst right)
  Binary ModOp left right -> liftA2 mod (isConst left) (isConst right)
  Binary BitAnd left right -> liftA2 (.&.) (isConst left) (isConst right)
  Binary BitOr left right -> liftA2 (.|.) (isConst left) (isConst right)
  Binary BitXor left right -> liftA2 xor (isConst left) (isConst right)
  Binary BitShr left right -> liftA2 shift (isConst left) ((* (-1)) <$> isConst right)
  Binary BitShl left right -> liftA2 shift (isConst left) (isConst right)
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
    isConst c >>= (\x -> if x /= 0 then isConst trueExpr else isConst falseExpr)