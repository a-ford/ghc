--------------------------------------------------------------------------------
-- | Conversions from backend types to llvm-general AST types
--

module Llvm.TypeConversions where

import Llvm.AbsSyn
import Llvm.MetaData
import Llvm.Types

import LLVM.General.AST

import Unique

-- Many of these functions look kind of like a non-polymorphic id

llvmLinkageTypeToLinkage :: LlvmLinkageType -> Linkage
llvmLinkageTypeToLinkage link =
    case link of
      Internal -> Internal
      LinkOnce -> LinkOnce
      Weak -> Weak
      Appending -> Appending
      ExternWeak -> ExternWeak
      ExternallyVisible -> AvailableExternally
      External -> External
      Private -> Private

llvmVarToGlobal :: LlvmVar -> Global
llvmVarToGlobal | (LMGlobalVar str ty link sec ali con) =
                    GlobalVariable {
                    name = (Name (unpackFS str)),
                    linkage = (llvmLinkageTypeToLinkage link)
                    visibility = Default,
                    isThreadLocal = False,
                    addrSpace = 0,
                    hasUnnamedAddr = False, --?
                    isConstant = (con == Constant),
                    type' = (llvmTypeToType ty),
                    initializer = Nothing, --?
                    section = sec >>= (Just . unpackFS),
                    alignment = if ali==Nothing then 0 else fromJust ali
                  }
                | (LMLocalVar uniq ty) = undefined
                | (LMNLocalVar str ty) = undefined
                | (LMLitVar lit) = undefined


floatToSomeFloat :: Double -> LlvmType -> SomeFloat
floatToSomeFloat d ty =
    case ty of
      LMFloat    -> Single d
      LMDouble   -> Double d
      LMFloat80  -> X86_FP80 {- need to split into a 16 and 64 bit word -}
      LMFloat128 -> Quadruple {- need to split into two 64 bit words -}
      _          -> error (show ty) ++ " is not an floating type."

llvmTypeToType :: LlvmType -> Type
llvmTypeToType
    | LMInt width = IntegerType width
    | LMFloat = FloatingPointType 32 IEEE
    | LMDouble = FloatingPointType 64 IEEE
    | LMFloat80 = FloatingPointType 80 DoubleExtended
    | LMFloat128 = FloatingPointType 128 IEEE
    | LMPointer ty = PointerType (llvmTypeToType ty) (AddrSpace 0) -- don't know about address space
    | LMArray len ty = ArrayType len (llvmTypeToType ty)
    | LMVector len typ = VectorType len (llvmTypeToType ty)
    | LMLabel = undefined
    | LMVoid = VoidType
    | LMStruct tys = StructureType false (map llvmTypeToType tys) -- not packed
    | LMAlias ali = NamedTypeReference
    | LMMetadata = MetaDataType
    | LMFunction decl@(name link cc ty vArgs params ali) = FunctionType (llvmTypeToType ty) (map (llvmTypeToType . fst) params) (vArgs == VarArgs)

llvmStaticToConstant :: LlvmStatic -> Constant
llvmStaticToConstant | LMComment str = undefined
                     | LMStaticLit lit = 
                         case lit of
                           LMIntLit i width -> Int width i
                           LMFloatLit d ty  -> Float (floatToSomeFloat d ty)
                           LMNullLit ty     -> Null (llvmTypeToType ty)
                           LMVectorLit lits -> Vector (map (llvmStaticToConstant . LlvmStaticLit) lits)
                           LMUndefLit       -> Undef VoidType
                       | LMUninitType ty = Undef (llvmTypeToType ty)
                       | LMStaticStr str ty = 
                       | LMStaticArray stats ty = Array (llvmTypeToType ty) (map llvmStaticToConstant stats)
                       | LMStaticStruc stats ty = Vector (map llvmStaticToConstant stats)
                       | LMStaticPointer var = undefined
                       -- static expressions
                       | LMBitc stat ty = BitCast (llvmStaticToConstant stat) (llvmTypeToType ty)
                       | LMPtoI stat ty = IntToPtr (llvmStaticToConstant stat) (llvmTypeToType ty)
                       | LMAdd statL statR = Add False False (llvmStaticToConstant statL) (llvmStaticToConstant statR) -- bools are for no (un)signed wrap
                       | LMSub statL statR = Sub False False (llvmStaticToConstant statL) (llvmStaticToConstant statR) -- bools are for no (un)signed wrap

llvmCallConventionToCallingConvention :: LlvmCallConvention -> CallingConvention
llvmCallConventionToCallingConvention conv =
    case conv of
      CC_Ccc -> C
      CC_Fastcc -> Fast
      CC_Coldcc -> Cold
      CC_Ncc 10 -> GHC
      CC_X86_Stdcc -> Numbered 64
      CC_Ncc code -> Numbered code

llvmFuncAttrToFunctionAttribute :: LlvmFuncAttr -> FunctionAttribute
llvmFuncAttrToFunctionAttribute attr =
    case attr of
      AlwaysInline -> AlwaysInline
      InlineHint -> InlineHint
      NoInline -> NoInline
      OptSize -> OptimizeForSize
      NoReturn -> NoReturn
      NoUnwind -> NoUnwind
      ReadNone -> ReadNone
      ReadOnly -> ReadOnly
      Ssp -> StackProtect
      SspReq -> StackProtectReq
      NoRedZone -> NoRedZone
      NoImplicitFloat -> NoImplicitFloat
      Naked -> Naked

llvmParamAttrToParameterAttribute :: LlvmParamAttr -> ParameterAttribute
llvmParamAttrToParameterAttribute attr =
    case attr of
      ZeroExt -> ZeroExt
      SignExt -> SignExt
      InReg -> InReg
      ByVal -> ByVal
      SRet -> SRet
      NoAlias -> NoAlias
      NoCapture -> NoCapture
      Nest -> Nest

-- Convert comparator operators to integer predicates
llvmCmpOpToIntPredicate :: LlvmCmpOp -> IntegerPredicate
llvmCmpOpToIntPredicate op =
    case op of
      LM_CMP_Eq  -> EQ
      LM_CMP_Ne  -> NE
      LM_CMP_Ugt -> UGT
      LM_CMP_Uge -> UGE
      LM_CMP_Ult -> ULT
      LM_CMP_Ule -> ULE
      LM_CMP_Sgt -> SGT
      LM_CMP_Sge -> SGE
      LM_CMP_Slt -> SLT
      LM_CMP_Sle -> SLE
      _          -> error $ (show op) ++ " is not an integer comparator."


-- The difference between O and U prefixed predicates relates to qNaN (quiet NaN) values
llvmCmpOpToFloatingPointPredicate :: LlvmCmpOp -> FloatingPointPredicate
llvmCmpOpToFloatingPointPredicate op =
    case op of
      LM_CMP_Feq -> OEQ
      LM_CMP_Fne -> ONE
      LM_CMP_Fgt -> OGT
      LM_CMP_Fge -> OGE
      LM_CMP_Flt -> OLT
      LM_CMP_Fle -> OLE
      _          -> error $ (show op) ++ " is not an floating point comparator."


-- LlvmCastOp conversions
llvmCastOpToInstruction :: LlvmCastOp -> LlvmType -> LlvmVar -> Instruction
llvmCastOpToInstruction op tyTo arg@(LMGlobalVar _ tyFrom _ _ _ _) =
    (case op of
      LM_Trunc    -> Trunc
      LM_Zext     -> ZExt
      LM_Sext     -> SExt
      LM_Fptrunc  -> FPTrunc
      LM_Fpext    -> FPToUI
      LM_Fptoui   -> FPToUI
      LM_Fptosi   -> FPToSI
      LM_Uitofp   -> UIToFP
      LM_Sitofp   -> SIToFP
      LM_Ptrtoint -> PtrToInt
      LM_Inttoptr -> IntToPtr
      LM_Bitcast  -> BitCast)
             $ (llvmVarToOperand op) (llvmTypeToType typeTo) [] -- ignore metadata for now

llvmVarToOperand :: LlvmVar -> Operand
llvmVarToOperand
    | (LMGlobalVar str ty link sec ali con) = LocalReference (Name (unpackFS str)) -- this isn't right, maybe this should just be undefined
    | (LMLocalVar uniq ty) = LocalReference (UnName uniq) -- types might not match up here
    | (LMNLocalVar str ty) = LocalReference (Name (unpackFS str))
    | (LMLitVar lit) = ConstantOperand (llvmStaticToConstant (LMStaticLit lit))

