--------------------------------------------------------------------------------
-- | Call into the Haskell LLVM API to generate LLVM bitcode.
--

module Llvm.CodeOutput where

import Llvm.AbsSyn
import Llvm.MetaData
import Llvm.Types

import LLVM.Core as LC
import LLVM.Core.Util as LCUtil
import LLVM.Core.Instructions as LCI
import LLVM.FFI.Core as LFC
import LLVM.Wrapper.Core as LWC

import Unique

--------------------------------------------------------------------------------
-- * Top Level Code Output functions
--------------------------------------------------------------------------------


outputLlvmModule :: LlvmModule -> IO LCUtil.Module
outputLlvmModule (LlvmModule comments aliases meta globals decls funcs) =
    

outputLlvmGlobal :: LMGlobal -> IO LCUtil.TGlobal a
outputLlvmGlobal (LMGlobal var@(LMGlobalVar name ty link sec ali con) dat) =


--------------------------------------------------------------------------------
-- * Top Level Output functions
--------------------------------------------------------------------------------

-- | Output out a whole LLVM module.
outputLlvmModule :: LlvmModule -> IO LCUtil.Module
outputLlvmModule (LlvmModule comments aliases meta globals decls funcs)

-- | Output out a list of global mutable variable definitions
outputLlvmGlobals :: [LMGlobal] -> 
outputLlvmGlobals ls = 

-- | Output out a global mutable variable definition
outputLlvmGlobal :: LMGlobal -> IO LCUtil.TGlobal a
outputLlvmGlobal (LMGlobal var@(LMGlobalVar name ty link x a c) dat) =
    

outputLlvmGlobal (LMGlobal var val) = sdocWithDynFlags $ \dflags ->
  error $ "Non Global var outputr as global! "
          ++ showSDoc dflags (outputr var) ++ " " ++ showSDoc dflags (outputr val)

-- | Output out a list of LLVM type aliases.
outputLlvmAliases :: [LlvmAlias] -> 
outputLlvmAliases tys = 

-- | Output out an LLVM type alias.
-- Can't find this in the API
outputLlvmAlias :: LlvmAlias -> 
outputLlvmAlias (name, ty) = 


-- | Output out a list of LLVM metadata.
outputLlvmMetas :: [MetaDecl] -> 
outputLlvmMetas metas = 

-- | Output out an LLVM metadata definition
outputLlvmMeta :: MetaDecl -> 
outputLlvmMeta (MetaUnamed n m) =

outputLlvmMeta (MetaNamed n m)
  =

-- | Output out an LLVM metadata value.
outputLlvmMetaExpr :: MetaExpr -> 
outputLlvmMetaExpr (MetaStr    s ) = 
outputLlvmMetaExpr (MetaNode   n ) = 
outputLlvmMetaExpr (MetaVar    v ) = 
outputLlvmMetaExpr (MetaStruct es) = 


-- | Output out a list of function definitions.
outputLlvmFunctions :: LlvmFunctions -> 
outputLlvmFunctions funcs = 

-- | Output out a function definition.
outputLlvmFunction :: LlvmFunction -> 
outputLlvmFunction (LlvmFunction dec args attrs sec body) =
    do
      f <- createFunction 

-- | Output out a function defenition header.
outputLlvmFunctionHeader :: LlvmFunctionDecl -> [LMString] -> 
outputLlvmFunctionHeader (LlvmFunctionDecl n l c r varg p a) args
  = 

-- | Output out a list of function declaration.
outputLlvmFunctionDecls :: LlvmFunctionDecls -> 
outputLlvmFunctionDecls decs = 

-- | Output out a function declaration.
-- Declarations define the function type but don't define the actual body of
-- the function.
outputLlvmFunctionDecl :: LlvmFunctionDecl -> 
outputLlvmFunctionDecl (LlvmFunctionDecl n l c r varg p a)
  = 

-- | Output out a list of LLVM blocks.
outputLlvmBlocks :: LlvmBlocks -> 
outputLlvmBlocks blocks = 

-- | Output out an LLVM block.
-- It must be part of a function definition.
outputLlvmBlock :: LlvmBlock -> 
outputLlvmBlock (LlvmBlock blockId stmts) = 

-- | Output out an LLVM block label.
outputLlvmBlockLabel :: LlvmBlockId -> 
outputLlvmBlockLabel id = 

-- | Output out an LLVM statement.
outputLlvmStatement :: LlvmStatement -> 
outputLlvmStatement stmt = 
  case stmt of
    Assignment  dst expr      -> --ind $ ppAssignment dst expr
    Fence       st ord        -> --ind $ ppFence st ord
    Branch      target        -> --ind $ ppBranch target
    BranchIf    cond ifT ifF  -> --ind $ ppBranchIf cond ifT ifF
    Comment     comments      -> --ind $ ppLlvmComments comments
    MkLabel     label         -> --ppLlvmBlockLabel label
    Store       value ptr     -> --ind $ ppStore value ptr
    Switch      scrut def tgs -> --ind $ ppSwitch scrut def tgs
    Return      result        -> --ind $ ppReturn result
    Expr        expr          -> --ind $ ppLlvmExpression expr
    Unreachable               -> --ind $ text "unreachable"
    Nop                       -> --empty
    MetaStmt    meta s        -> --ppMetaStatement meta s

-- | Output out an LLVM expression.
outputLlvmExpression :: LlvmExpression -> 
outputLlvmExpression expr
  = 

--------------------------------------------------------------------------------
-- * Individual print functions
--------------------------------------------------------------------------------

-- N.B. type Terminate = ()

-- | Should always be a function pointer. So a global var of function type
-- (since globals are always pointers) or a local var of pointer function type.
-- LFC.setTailCall if ct == TailCall
outputCall :: LlvmCallType -> LlvmVar -> [MetaExpr] -> [LlvmFuncAttr] -> 
outputCall ct fptr args attrs = 

outputMachOp :: LlvmMachOp -> LlvmVar -> LlvmVar -> 
outputMachOp op left right = 

outputCmpOp :: LlvmCmpOp -> LlvmVar -> LlvmVar -> 
outputCmpOp op left right = 

-- check types here
outputAssignment :: LlvmVar -> LlvmExpression -> 
outputAssignment var expr = outputName var <+> equals <+> (ppLlvmExpression expr)

-- Can't find a binding for this
outputFence :: Bool -> LlvmSyncOrdering -> 
outputFence st ord = 

-- Nope, doesn't seem to be supported
outputSyncOrdering :: LlvmSyncOrdering -> 
outputSyncOrdering SyncUnord     = 
outputSyncOrdering SyncMonotonic = 
outputSyncOrdering SyncAcquire   = 
outputSyncOrdering SyncRelease   = 
outputSyncOrdering SyncAcqRel    = 
outputSyncOrdering SyncSeqCst    = 

-- XXX: On x86, vector types need to be 16-byte aligned for aligned access, but
-- we have no way of guaranteeing that this is true with GHC (we would need to
-- modify the layout of the stack and closures, change the storage manager,
-- etc.). So, we blindly tell LLVM that *any* vector store or load could be
-- unaligned. In the future we may be able to guarantee that certain vector
-- access patterns are aligned, in which case we will need a more granular way
-- of specifying alignment.

outputLoad :: LlvmVar -> 
outputLoad var
    -- n.b. no alignment data, should have alignment=1 byte
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


outputCast :: LlvmCastOp -> LlvmVar -> LlvmType -> 
outputCast op from to = 

-- mallocing in the LLVM API requires 
outputMalloc :: LlvmType -> Int -> 
outputMalloc tp amount = LCI.arrayMalloc ((llvmTypeToType tp)::amount)

outputAlloca :: LlvmType -> Int -> 
outputAlloca tp amount = 

outputGetElementPtr :: Bool -> LlvmVar -> [LlvmVar] -> 
outputGetElementPtr inb ptr idx = 

outputReturn :: Maybe LlvmVar -> 
outputReturn (Just var) = LCI.ret (outputVar var)
outputReturn Nothing    = LCI.ret ()

-- Unconditional branch to target
outputBranch :: LlvmVar -> CodeGenFunction r Terminate
outputBranch var = LCI.br (outputVar var)


outputBranchIf :: LlvmVar -> LlvmVar -> LlvmVar -> CodeGenFunction r Terminate
outputBranchIf cond trueT falseT
  = LCI.condBr (llvmVarToBool cond) (outputVar trueT) (outputVar falseT)


outputPhi :: LlvmType -> [(LlvmVar,LlvmVar)] -> 
outputPhi tp preds = LCI.phi 


outputSwitch :: LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> 
outputSwitch scrut dflt targets = 

outputAsm :: LMString -> LMString -> LlvmType -> [LlvmVar] -> Bool -> Bool -> 
outputAsm asm constraints rty vars sideeffect alignstack = LCI.switch 

-- Get a value from a vector
outputExtract :: LlvmVar -> LlvmVar -> 
outputExtract vec idx = 

-- Insert a value into a vector
outputInsert :: LlvmVar -> LlvmVar -> LlvmVar -> 
outputInsert vec elt idx = LCI.insertElement vec elt idx

outputMetaStatement :: [MetaAnnot] -> LlvmStatement -> 
outputMetaStatement meta stmt = 

outputMetaExpr :: [MetaAnnot] -> LlvmExpression -> 
outputMetaExpr meta expr = 

outputMetaAnnots :: [MetaAnnot] -> 
outputMetaAnnots meta = 

--------------------------------------------------------------------------------
-- * Misc functions
--------------------------------------------------------------------------------

-- | Blank line.
newLine :: SDoc
newLine = empty

-- | Exclamation point.
exclamation :: SDoc
exclamation = char '!'

llvmVarToValue :: LlvmVar -> LCU.Value
llvmVarToValue var = 

llvmVarToBasicBlock :: LlvmVar -> LCU.BasicBlock
llvmVarToBasicBlock | (LMGlobalVar str ty link sec ali con) =
                    | (LMLocalVar uniq ty) =
                    | (LMNLocalVar str ty) =
                    | (LMLitVar lit) =

llvmTypeToType :: LlvmType -> IO LWC.Type
llvmTypeToType ty =
    case ty of
    LMInt i -> LFC.integerType (CUInt i)
    LMFloat -> LFC.floatType
    LMDouble -> LFC.doubleType
    LMFloat80 -> LFC.x86FP80Type
    LMFloat128 -> LFC.fp128Type
    LMPointer ty' -> LFC.pointerType (llvmTypeToType ty') {-addr space?-}
    LMArray i ty' -> LFC.arrayType (llvmTypeToType ty') (CUInt i)
    LMVector i ty' -> LFC.pointerType (llvmTypeToType ty') (CUInt i)
    LMLabel -> LFC.labelType
    LMVoid -> LFC.voidType
    LMStruct [ty'] -> LFC.structType (llvmTypeToType ty') (CUInt {-?-}) (CInt 0) -- not packed
    LMAlias ali -> {- got nothing -}
    LMMetadata  -> {- got nothing -}

llvmCallConventionToCallingConvention :: LlvmCallConvention -> LFC.CallingConvention
llvmCallConventionToCallingConvention conv =
    case conv of
      CC_Ccc -> C
      CC_Fastcc -> Fast
      CC_Coldcc -> Cold
      CC_Ncc code -> LFC.toCallingConvention code
      CC_X86_Stdcc -> X86StdCall

llvmLinkageTypeToLinkage :: LlvmLinkageType -> LFC.Linkage
llvmLinkageTypeToLinkage link =
    case link of
         Internal -> ExternalLinkage
         LinkOnce -> LinkOnceAnyLinkage -- LinkOnceODRLinkage has different semantics
         Weak -> WeakAnyLinkage -- ditto WeakODRLinkage
         Appending -> AppendingLinkage
         ExternWeak -> ExternalWeakLinkage
         ExternallyVisible -> ExternalLinkage -- default
         External -> ExternalLinkage -- same as ExternallyVisible
         Private -> PrivateLinkage

llvmFuncAttrToAttribute :: LlvmFuncAttr -> LFC.Attribute
llvmFuncAttrToAttribute attr =
    case attr of
      AlwaysInline -> AlwaysInlineAttribute
      InlineHint -> InlineHintAttribute
      NoInline -> NoInlineAttribute
      OptSize -> OptimizeForSizeAttribute
      NoReturn -> NoReturnAttribute
      NoUnwind -> NoUnwindAttribute
      ReadNone -> ReadNoneAttribute
      ReadOnly -> ReadOnlyAttribute
      Ssp -> StackProtectAttribute
      SspReq -> StackProtectReqAttribute
      NoRedZone -> NoRedZoneAttribute
      NoImplicitFloat -> NoImplicitFloatAttribute
      Naked -> NakedAttribute

-- FFI Attributes account for both function and parameter attributes
llvmParamAttrTo :: LlvmParamAttr -> LFC.Attribute
llvmParamAttrTo attr =
    case attr of
      ZeroExt -> ZExtAttribute
      SignExt -> SExtAttribute
      InReg -> InRegAttribute
      ByVal -> ByValAttribute
      SRet -> StructRetAttribute
      NoAlias -> NoAliasAttribute
      NoCapture -> NoCaptureAttribute
      Nest -> NestAttribute

-- these are all binops, types should but are not constrained
llvmMachOpToCodeGenFunction :: LlvmMachOp -> (a -> b -> CodeGenFunction r (v c))
llvmMachOpToCodeGenFunction op =
    case op of
      LM_MO_Add  -> LCI.add
      LM_MO_Sub  -> LCI.sub
      LM_MO_Mul  -> LCI.mul
      LM_MO_UDiv -> LCI.idiv -- udiv is deprecated
      LM_MO_SDiv -> LCI.idiv -- sdiv is deprecated
      LM_MO_URem -> LCI.irem -- urem is deprecated
      LM_MO_SRem -> LCI.irem -- srem is deprecated
      LM_MO_FAdd -> LCI.fadd
      LM_MO_FSub -> LCI.fsub
      LM_MO_FMul -> LCI.fmul
      LM_MO_FDiv -> LCI.fdiv
      LM_MO_FRem -> LCI.frem
      LM_MO_Shl  -> LCI.shl
      LM_MO_LShr -> LCI.lshr
      LM_MO_AShr -> LCI.ashr
      LM_MO_And  -> LCI.and
      LM_MO_Or   -> LCI.or
      LM_MO_Xor  -> LCI.xor

-- Convert comparator operators to integer predicates
llvmCmpOpToIntPredicate :: LlvmCmpOp -> LCI.IntPredicate
llvmCmpOpToIntPredicate op =
    case op of
      LM_CMP_Eq  -> IntEQ
      LM_CMP_Ne  -> IntNE
      LM_CMP_Ugt -> IntUGT
      LM_CMP_Uge -> IntUGE
      LM_CMP_Ult -> IntULT
      LM_CMP_Ule -> IntULE
      LM_CMP_Sgt -> IntSGT
      LM_CMP_Sge -> IntSGE
      LM_CMP_Slt -> IntSLT
      LM_CMP_Sle -> IntSLE
      _          -> error $ (show op) ++ " is not an integer comparator."

-- Convert comparator operators to floating point predicates
llvmCmpOpToFPPredicate :: LlvmCmpOp -> LCI.FPPredicate
llvmCmpOpToFPPredicate op =
    case op of
      LM_CMP_Feq -> FPOEQ
      LM_CMP_Fne -> FPONE
      LM_CMP_Fgt -> FPOGT
      LM_CMP_Fge -> FPOGE
      LM_CMP_Flt -> FPOLT
      LM_CMP_Fle -> FPOLE
      _          -> error $ (show op) ++ " is not an floating point comparator."


-- LlvmCastOp conversions
llvmCastOpToCast :: LlvmCastOp -> (LWC.Builder -> LWC.Value -> LWC.Type -> LWC.String -> IO LWC.Value)
llvmCastOpToCast op =
    case op of
      LM_Trunc    -> LWC.buildTrunc
      LM_Zext     -> LWC.buildZExt
      LM_Sext     -> LWC.buildSExt
      LM_Fptrunc  -> LWC.buildFPTrunc
      LM_Fpext    -> LWC.buildFPExt
      LM_Fptoui   -> LWC.buildFPToUI
      LM_Fptosi   -> LWC.buildFPToSI
      LM_Uitofp   -> LWC.buildUIToFP
      LM_Sitofp   -> LWC.buildSItoFP
      LM_Ptrtoint -> LWC.buildPtrToInt
      LM_Inttoptr -> LWC.buildIntToPtr
      LM_Bitcast  -> LWC.buildBitCast

-- LlvmStatic (global variables and constants) conversion
llvmStaticTo