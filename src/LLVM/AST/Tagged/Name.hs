{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides a type-safe variant of "LLVM.AST.Name".
-- It is currently a stub
module LLVM.AST.Tagged.Name (
  Name,
  UnNamed,

  Named(..),

  name,
  unname,

  verifyName,
  verifyNamed,
) where

import LLVM.Prelude
import LLVM.AST.Tagged.Tag
import qualified LLVM.AST.Name
import qualified LLVM.AST.Instruction

data TNamed = TName | TUnNamed
type NameAnn (a :: TNamed) = LLVM.AST.Name.Name

type Name = NameAnn TName
type UnNamed = NameAnn TUnNamed

name :: ShortByteString -> Name
name = LLVM.AST.Name.Name

unname :: ShortByteString -> UnNamed
unname = LLVM.AST.Name.Name

data Named a
  = Name := a
  | Do a
  deriving (Eq, Read, Show, Typeable, Data, Generic)

verifyName :: NameAnn a -> LLVM.AST.Name.Name
verifyName = id

verifyNamed :: Named a -> LLVM.AST.Instruction.Named a
verifyNamed (nm := a) = nm LLVM.AST.Instruction.:= a
verifyNamed (Do a) = LLVM.AST.Instruction.Do a
