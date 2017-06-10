-- | This module provides a type-safe variant of "LLVM.AST.Global".
-- It is currently a stub
module LLVM.AST.Tagged.Global (
  {-module LLVM.AST.Global,-}
  basicBlock,
) where

import LLVM.AST.Global
import LLVM.AST.Instruction (Instruction, Terminator)
import LLVM.AST.Tagged.Name

basicBlock :: Name -> [Named Instruction] -> (Named Terminator) -> BasicBlock
basicBlock nm instr term = BasicBlock (verifyName nm) (fmap verifyNamed instr) (verifyNamed term)
