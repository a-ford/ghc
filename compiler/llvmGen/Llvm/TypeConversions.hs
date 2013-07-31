--------------------------------------------------------------------------------
-- | Conversions from backend types to Haskell LLVM bindings types
--

module Llvm.TypeConversions where

import Llvm.AbsSyn
import Llvm.MetaData
import Llvm.Types

import LLVM.Core as LC
import LLVM.Core.Util as LCUtil
import LLVM.Core.Instructions as LCI
import LLVM.Core.Type as LCT
import LLVM.FFI.Core as LFC
import LLVM.Wrapper.Core as LWC

import Unique

llvmVarToValue :: LlvmVar -> LCU.Value
llvmVarToValue  | (LMGlobalVar str ty link sec ali con) =
                    if con == Constant then LC.valueOf 
{-                    case type of
                      LMInt width        ->             -- ^ An integer with a given width in bits.
                      LMFloat            ->          -- ^ 32 bit floating point
                      LMDouble           ->          -- ^ 64 bit floating point
                      LMFloat80          ->          -- ^ 80 bit (x86 only) floating point
                      LMFloat128         ->          -- ^ 128 bit floating point
                      LMPointer ty       ->   -- ^ A pointer to a 'LlvmType'
                      LMArray size ty    ->  -- ^ An array of 'LlvmType'
                      LMVector size ty   -> -- ^ A vector of 'LlvmType'
                      LMLabel            ->     -- ^ A 'LlvmVar' can represent a label (address)
                      LMVoid             ->     -- ^ Void type
                      LMStruct tys       ->   -- ^ Structure type
                      LMAlias (name, ty) ->     -- ^ A type alias
                      LMMetadata         ->           -- ^ LLVM Metadata
                      LMFunction decl    ->
-}

                | (LMLocalVar uniq ty) = undefined
                | (LMNLocalVar str ty) = undefined
                | (LMLitVar lit) = undefined

llvmTypeToTypeDesc :: LlvmType -> LCT.TypeDesc
llvmTypeToTypeDesc ty =
    case ty of
      LMInt i -> TDInt False i -- not unsigned
      LMFloat -> TDFloat
      LMDouble -> TDDouble
      LMFloat80 -> error "Unimplemented"
      LMFloat128 -> TDFP128
      LMPointer ty' -> TDPtr (llvmTypetoTypeDesc ty')
      LMArray size ty' -> TDArrray size (llvmTypetoTypeDesc ty')
      LMVector size ty' -> TDVector size (llvmTypeToType ty')
      LMLabel -> TDLabel
      LMVoid -> TDVoid
      LMStruct tys' -> TDStruct (map llvmTypeToTypeDesc tys') False --not packed
      LMAlias ali -> error "Unimplemented"
      LMMetadata  -> error "Unimplemented"

llvmCallConventionToCallingConvention :: LlvmCallConvention -> LCI.CallingConvention
llvmCallConventionToCallingConvention conv =
    case conv of
      CC_Ccc -> C
      CC_Fastcc -> Fast
      CC_Coldcc -> Cold
      CC_Ncc 10 -> GHC
      CC_X86_Stdcc -> X86StdCall
      CC_Ncc code -> error "Invalid calling convention: " ++ (show code)
                     --LFC.toCallingConvention code, not supported in the high level bindings

llvmLinkageTypeToLinkage :: LlvmLinkageType -> LC.Linkage
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

llvmFuncAttrToAttribute :: LlvmFuncAttr -> LC.Attribute
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
llvmParamAttrTo :: LlvmParamAttr -> LC.Attribute
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
-- Higher level llvm bindings only expose bitcasts, why is this?
llvmCastOpToInstrDesc :: LlvmCastOp -> LlvmType -> LlvmVar -> LCI.InstrDesc
llvmCastOpToInstrDesc op tyTo arg@(LMGlobalVar _ tyFrom _ _ _ _) =
    (case op of
      LM_Trunc    -> IDTrunc
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
      LM_Bitcast  -> LWC.buildBitCast)
             $ (llvmTypeToTypeDesc tyFrom) (llvmTypeToTypeDesc tyTo) (llvmVarToArgDesc arg)

-- this might not be a thing
llvmVarToArgDesc :: LlvmVar -> LCI.ArgDesc
llvmVarToArgDesc var =
    case var of
      LMGlobalVar str ty link sec ali con -> -- presumably we need to look these up in the environment
      LMLocalVar uniq ty -> -- presumably we need to look these up in the environment
      LMNLocalVar str ty -> -- presumably we need to look these up in the environment
      LMLitVar lit -> (case lit of
                         LMintLit i _ -> AI i
                         LMFloatLit _ -> error "ArgDesc: Floats not supported."
                         LMNullLit _  -> AE
                         LMVectorLit  -> error "ArgDesc: Vectors not supported."
                         LMUndefLit _ -> error "ArgDesc: Undefined") -- N.B. This might not work


-- LlvmStatic (global variables and constants) conversion
llvmStaticToConstValue :: LlvmStatic -> LC.ConstValue
llvmStaticToConstValue | LMComment str = constOf str
                       | LMStaticLit lit = case lit of
                                             LMIntLit i width -> constOf $ intToIntN i width
                                             LMFloatLit d ty  -> constOf $ floatToFloatN d ty
                                             LMNullLit ty     -> (LC.zero :: (llvmTypeToType ty0))
                                             LMVectorLit lits -> constOf $ vector (map llvmStaticToConstValue lits)
                                             LMUndefLit       -> LC.undef
                       -- TODO
                       | LMUninitType ty = error "llvmStaticToConstValue: No uninitialised consts."
                       | LMStaticStr str ty = error "Constant strings are broken in the bindings."
                       | LMStaticArray stats ty = undefined
                       | LMStaticStruc stats ty = undefined
                       | LMStaticPointer var = undefined
                       -- static expressions
                       | LMBitc stat ty = undefined
                       | LMPtoI stat ty = undefined
                       | LMAdd statL statR = undefined
                       | LMSub statL statR = undefined

-- Convert Ints to (signed) Ints of a certain width
intToIntN :: Integral a => Int -> Int -> a
intToIntN i width = case width of
                      8  -> Int8 i
                      16 -> Int16 i
                      32 -> Int32 i
                      64 -> Int64 i
                      _  -> error $ "intToIntN: " ++ (show width) ++
                               " is not a valid integer width"

floatToFloatN :: Floating a :: Double -> LlvmType -> a
floatToFloatN d ty = case type of
                       LMFloat    -> (d :: Float)
                       LMDouble   -> (d :: Double)
                       LMFloat80  -> (d :: Double) -- We don't have appropriate
                       LMFloat128 -> (d :: Double) -- built in types for these
                       _          -> error $ "floatToFloatN: " ++ (show ty) ++
                                     "is not an appropriate float type"

basicLit :: LlvmLit -> a
basicLit | LMIntLit i width = intToIntN i width
         | LMFloatLit d ty  = floatToFloatN d ty
         | LMNullLit ty     = error "basicLit: nulls unsupported"
         | LMVectorLit lits = vector (map basicLit lits)
         | LMUndefLit       = error "basicLit: undefs unsupported"