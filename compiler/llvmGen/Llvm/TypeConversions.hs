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
llvmVarToValue  | (LMGlobalVar str ty link sec ali con) = undefined
                | (LMLocalVar uniq ty) = undefined
                | (LMNLocalVar str ty) = undefined
                | (LMLitVar lit) = undefined

llvmVarToBasicBlock :: LlvmVar -> LCU.BasicBlock
llvmVarToBasicBlock | (LMGlobalVar str ty link sec ali con) = undefined
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
--llvmStaticTo


--llvmStaticToConstValue :: LlvmStatic -> LC

