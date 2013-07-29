--------------------------------------------------------------------------------
-- | Call into the Haskell LLVM API to generate LLVM bitcode.
--

module Llvm.CodeOutput where

import Llvm.AbsSyn
import Llvm.MetaData
import Llvm.Types

import LLVM.Core as LC
import LLVM.Core.Util as LCU
import LLVM.Core.Instructions as LCI
import LLVM.Core.Type as LCT
import LLVM.FFI.Core as LFC
import LLVM.Wrapper.Core as LWC

import Unique

--------------------------------------------------------------------------------
-- * Top Level Code Output functions
--------------------------------------------------------------------------------


outputLlvmModule :: LlvmModule -> IO LCU.Module
outputLlvmModule (LlvmModule comments aliases meta globals decls funcs) = undefined

outputLlvmGlobal :: LMGlobal -> IO LC.TGlobal a
outputLlvmGlobal (LMGlobal var@(LMGlobalVar name ty link sec ali con) dat) = 
    createGlobal (con==Const) (llvmLinkageTypeToLinkage link) (llvmStaticToConstValue (fromJust dat))


--------------------------------------------------------------------------------
-- * Top Level Output functions
--------------------------------------------------------------------------------

-- | Output out a whole LLVM module.
outputLlvmModule :: LlvmModule -> IO LCU.Module
outputLlvmModule (LlvmModule comments aliases meta globals decls funcs) =  undefined

-- | Output out a list of global mutable variable definitions
outputLlvmGlobals :: [LMGlobal] -> 
outputLlvmGlobals ls =  undefined

-- | Output out a global mutable variable definition
outputLlvmGlobal :: LMGlobal -> IO LCU.TGlobal a
outputLlvmGlobal (LMGlobal var@(LMGlobalVar name ty link x a c) dat) = undefined
    

outputLlvmGlobal (LMGlobal var val) = sdocWithDynFlags $ \dflags ->
  error $ "Non Global var outputr as global! "
          ++ showSDoc dflags (outputr var) ++ " " ++ showSDoc dflags (outputr val)

-- | Output out a list of LLVM type aliases.
outputLlvmAliases :: [LlvmAlias] -> 
outputLlvmAliases tys =  undefined

-- | Output out an LLVM type alias.
-- Can't find this in the API
outputLlvmAlias :: LlvmAlias -> 
outputLlvmAlias (name, ty) =  undefined


-- | Output out a list of LLVM metadata.
outputLlvmMetas :: [MetaDecl] -> 
outputLlvmMetas metas =  undefined

-- | Output out an LLVM metadata definition
outputLlvmMeta :: MetaDecl -> 
outputLlvmMeta (MetaUnamed n m) = undefined

outputLlvmMeta (MetaNamed n m)
  = undefined

-- | Output out an LLVM metadata value.
outputLlvmMetaExpr :: MetaExpr -> 
outputLlvmMetaExpr (MetaStr    s ) = undefined 
outputLlvmMetaExpr (MetaNode   n ) = undefined 
outputLlvmMetaExpr (MetaVar    v ) = undefined 
outputLlvmMetaExpr (MetaStruct es) = undefined 


-- | Output out a list of function definitions.
outputLlvmFunctions :: LlvmFunctions -> 
outputLlvmFunctions funcs = undefined 

-- | Output out a function definition.
outputLlvmFunction :: LlvmFunction -> 
outputLlvmFunction (LlvmFunction dec args attrs sec body) = undefined
    do
      f <- createFunction 

-- | Output out a function defenition header.
outputLlvmFunctionHeader :: LlvmFunctionDecl -> [LMString] -> 
outputLlvmFunctionHeader (LlvmFunctionDecl n l c r varg p a) args
  = undefined 

-- | Output out a list of function declaration.
outputLlvmFunctionDecls :: LlvmFunctionDecls -> 
outputLlvmFunctionDecls decs = undefined 

-- | Output out a function declaration.
-- Declarations define the function type but don't define the actual body of
-- the function.
outputLlvmFunctionDecl :: LlvmFunctionDecl -> 
outputLlvmFunctionDecl (LlvmFunctionDecl n l c r varg p a)
  = undefined 

-- | Output out a list of LLVM blocks.
outputLlvmBlocks :: LlvmBlocks -> 
outputLlvmBlocks blocks = undefined 

-- | Output out an LLVM block.
-- It must be part of a function definition.
outputLlvmBlock :: LlvmBlock -> 
outputLlvmBlock (LlvmBlock blockId stmts) = undefined 

-- | Output out an LLVM block label.
outputLlvmBlockLabel :: LlvmBlockId -> 
outputLlvmBlockLabel id = undefined 

-- | Output out an LLVM statement. The types here may not match up.
outputLlvmStatement :: LlvmStatement -> 
outputLlvmStatement stmt = 
  case stmt of
    Assignment  dst expr      -> outputAssignment dst expr
    Fence       st ord        -> outputFence st ord
    Branch      target        -> outputBranch target
    BranchIf    cond ifT ifF  -> outputBranchIf cond ifT ifF
    Comment     comments      -> outputLlvmComments comments
    MkLabel     label         -> outputLlvmBlockLabel label
    Store       value ptr     -> outputStore value ptr
    Switch      scrut def tgs -> outputSwitch scrut def tgs
    Return      result        -> outputReturn result
    Expr        expr          -> outputLlvmExpression expr
    Unreachable               -> LCI.unreachable
    Nop                       -> 
    MetaStmt    meta s        -> outputMetaStatement meta s

-- | Output out an LLVM expression. Same potential problem with the types here,
-- need to work out what types will be. Probably CodeGenFunction.
outputLlvmExpression :: LlvmExpression -> 
outputLlvmExpression expr
  = case expr of
      Alloca     tp amount        -> outputAlloca tp amount
      LlvmOp     op left right    -> outputMachOp op left right
      Call       tp fp args attrs -> outputCall tp fp (map MetaVar args) attrs
      CallM      tp fp args attrs -> outputCall tp fp args attrs
      Cast       op from to       -> outputCast op from to
      Compare    op left right    -> outputCmpOp op left right
      Extract    vec idx          -> outputExtract vec idx
      Insert     vec elt idx      -> outputInsert vec elt idx
      GetElemPtr inb ptr indexes  -> outputGetElementPtr inb ptr indexes
      Load       ptr              -> outputLoad ptr
      Malloc     tp amount        -> outputMalloc tp amount
      Phi        tp precessors    -> outputPhi tp precessors
      Asm        asm c ty v se sk -> outputAsm asm c ty v se sk
      MExpr      meta expr        -> outputMetaExpr meta expr


--------------------------------------------------------------------------------
-- * Individual print functions
--------------------------------------------------------------------------------

-- N.B. type Terminate = undefined ()

-- | Should always be a function pointer. So a global var of function type
-- (since globals are always pointers) or a local var of pointer function type.
-- LFC.setTailCall if ct = TailCall
-- Ignore tail calls for now, the optimizer /should/ take care of them.
outputCall :: LlvmCallType -> LlvmVar -> [MetaExpr] -> [LlvmFuncAttr] -> 
outputCall ct fptr args attrs = undefined -- doCallDef 

outputMachOp :: LlvmMachOp -> LlvmVar -> LlvmVar -> 
outputMachOp op left right = op' (llvmVarToValue left) (llvmVarToValue right)
    where op' = llvmMachOpToCodeGenFunction op

outputCmpOp :: LlvmCmpOp -> LlvmVar -> LlvmVar -> 
outputCmpOp op left right = llvmCmpOpToIntPredicate op

-- check types here, can't find assignment op
outputAssignment :: LlvmVar -> LlvmExpression -> 
outputAssignment var expr = undefined

-- Can't find a binding for this
outputFence :: Bool -> LlvmSyncOrdering -> 
outputFence st ord = undefined 

-- Or for this
outputSyncOrdering :: LlvmSyncOrdering -> 
outputSyncOrdering SyncUnord     = undefined 
outputSyncOrdering SyncMonotonic = undefined 
outputSyncOrdering SyncAcquire   = undefined 
outputSyncOrdering SyncRelease   = undefined 
outputSyncOrdering SyncAcqRel    = undefined 
outputSyncOrdering SyncSeqCst    = undefined 

-- XXX: On x86, vector types need to be 16-byte aligned for aligned access, but
-- we have no way of guaranteeing that this is true with GHC (we would need to
-- modify the layout of the stack and closures, change the storage manager,
-- etc.). So, we blindly tell LLVM that *any* vector store or load could be
-- unaligned. In the future we may be able to guarantee that certain vector
-- access patterns are aligned, in which case we will need a more granular way
-- of specifying alignment.

outputLoad :: LlvmVar -> 
outputLoad var
    -- n.b. no alignment data, should have alignment= undefined1 byte
    | isVecPtrVar var = LCI.load (llvmVarToValue var)
    | otherwise       = LCI.load (llvmVarToValue var)
  where
    isVecPtrVar :: LlvmVar -> Bool
    isVecPtrVar = isVector . pLower . getVarType

outputStore :: LlvmVar -> LlvmVar -> 
outputStore val dst
    | isVecPtrVar dst = LCI.store val dst -- again, we need 1 byte alignment here
    | otherwise       = LCI.store val dst
  where
    isVecPtrVar :: LlvmVar -> Bool
    isVecPtrVar = isVector . pLower . getVarType


outputCast :: LlvmCastOp -> LlvmVar -> LlvmType -> LCI.InstrDesc
outputCast op var ty = llvmCastOpToInstrDesc op ty var

-- mallocing in the LLVM API requires 
outputMalloc :: LlvmType -> Int -> 
outputMalloc tp amount = LCI.arrayMalloc ((llvmTypeToType tp)::amount)

outputAlloca :: LlvmType -> Int -> 
outputAlloca tp amount = LCI.alloca -- ...

outputGetElementPtr :: Bool -> LlvmVar -> [LlvmVar] -> 
outputGetElementPtr inb ptr idx = undefined

outputReturn :: Maybe LlvmVar -> 
outputReturn (Just var) = LCI.ret (llvmVarToValue var)
outputReturn Nothing    = LCI.ret ()

-- Unconditional branch to target
outputBranch :: LlvmVar -> CodeGenFunction r Terminate
outputBranch var = LCI.br (llvmVarToBasicBlock var)


outputBranchIf :: LlvmVar -> LlvmVar -> LlvmVar -> CodeGenFunction r Terminate
outputBranchIf cond trueT falseT
  = LCI.condBr (llvmVarToBool cond) (llvmVarToBasicBlock trueT) (llvmVarToBasicBlock falseT)


outputPhi :: LlvmType -> [(LlvmVar,LlvmVar)] -> 
outputPhi tp preds = LCI.phi -- ...


outputSwitch :: LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> 
outputSwitch scrut dflt targets = LCI.switch (llvmVarToValue scrut) 
                                    (llvmVarToBasicBlock dflt) 
                                    (map convertTarget targets)
    where convertTarget = (\(con, target) -> (llvmVarToConst con, llvmVarToBasicBlock target))


-- Look to LFC.constInlineAsm for this, no high level bindings
outputAsm :: LMString -> LMString -> LlvmType -> [LlvmVar] -> Bool -> Bool -> 
outputAsm asm constraints rty vars sideeffect alignstack = undefined

-- Get a value from a vector
outputExtract :: LlvmVar -> LlvmVar -> 
outputExtract vec idx = 
    LCI.extractelement (llvmVarToValue vec) (llvmVarToValue idx)

-- Insert a value into a vector
outputInsert :: LlvmVar -> LlvmVar -> LlvmVar -> 
outputInsert vec elt idx = 
    LCI.insertElement (llvmVarToValue vec) (llvmVarToValue elt) (llvmVarToValue idx)

outputMetaStatement :: [MetaAnnot] -> LlvmStatement -> 
outputMetaStatement meta stmt = undefined 

outputMetaExpr :: [MetaAnnot] -> LlvmExpression -> 
outputMetaExpr meta expr = undefined 

outputMetaAnnots :: [MetaAnnot] -> 
outputMetaAnnots meta = undefined 