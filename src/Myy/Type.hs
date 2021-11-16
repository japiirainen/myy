{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}

-- Defines types

module Myy.Type
    ( Ty(..)
    , readTyCon
    , STy(..)
    , SCtx(..)
    , ITy(..)
    , emptyContext
    , refineTy
    , unrefineTy
    , eqSTy
    ) where

import           Myy.Util
import           Text.PrettyPrint.ANSI.Leijen

-- | Representation of a `Myy` type
data Ty
    = Arr Ty Ty
    -- ^ A function type
    | IntTy
    | BoolTy
    deriving stock Eq
infixr 1 `Arr`

-- | Perhaps convert a string representation of a base type into a `Ty
readTyCon :: String -> Maybe Ty
readTyCon "Int"  = Just IntTy
readTyCon "Bool" = Just BoolTy
readTyCon _      = Nothing

-- | Singleton for a myy type
data STy :: * -> * where
    SArr :: STy arg -> STy res -> STy (arg -> res)
    SIntTy :: STy Int
    SBoolTy :: STy Bool
infixr 1 `SArr`

-- | An implicit `STy`, wrapped up in a class constraint
class ITy ty where
    sty :: STy ty

instance (ITy arg, ITy res) => ITy (arg -> res) where
    sty = sty `SArr` sty
instance ITy Int where
    sty = SIntTy
instance ITy Bool where
    sty = SBoolTy

-- | Singleton instance for a typing context
data SCtx :: [*] -> * where
    SNil :: SCtx '[]
    SCons :: STy h -> SCtx t -> SCtx (h ': t)
infixr 5 `SCons`

-- | The singleton for the empty context
emptyContext :: SCtx '[]
emptyContext = SNil

-- | Convert a `Ty` into a `STy`.
refineTy :: Ty -> (forall ty. STy ty -> r) -> r
refineTy (ty1 `Arr` ty2) k
    = refineTy ty1 $ \sty1 ->
      refineTy ty2 $ \sty2 ->
      k (sty1 `SArr` sty2)
refineTy IntTy k = k SIntTy
refineTy BoolTy k = k SBoolTy

-- | Convert a `STy` into an `Ty
unrefineTy :: STy ty -> Ty
unrefineTy (arg `SArr` res) = unrefineTy arg `Arr` unrefineTy res
unrefineTy SIntTy           = IntTy
unrefineTy SBoolTy          = BoolTy

-- | Compare two `STy`s for equality
eqSTy :: STy ty1 -> STy ty2 -> Maybe (ty1 :~: ty2)
eqSTy (s1 `SArr` t1) (s2 `SArr` t2)
    | Just Refl <- eqSTy s1 s2
    , Just Refl <- eqSTy t1 t2
    = Just Refl
eqSTy SIntTy SIntTy = Just Refl
eqSTy SBoolTy SBoolTy = Just Refl
eqSTy _ _ = Nothing

-- Pretty-printing
instance Pretty Ty where
    pretty = pretty_ty topPrec

instance Show Ty where
    show = render . pretty

instance Pretty (STy ty) where
    pretty = pretty . unrefineTy

arrowLeftPrec, arrowRightPrec, arrowPrec :: Prec
arrowLeftPrec = 5
arrowRightPrec = 4.9
arrowPrec = 5

pretty_ty :: Prec -> Ty -> Doc
pretty_ty prec (Arr arg res) =
    maybeParens (prec >= arrowPrec) $
        hsep [ pretty_ty arrowLeftPrec arg
             , text "->"
             , pretty_ty arrowRightPrec res
             ]
pretty_ty _ IntTy = text "Int"
pretty_ty _ BoolTy = text "Bool"

