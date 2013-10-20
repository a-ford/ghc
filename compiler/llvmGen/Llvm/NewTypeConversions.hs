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
llvmVarToGlobal (LMGlobalVar str ty link sec ali con) =
                    GlobalVariable {
                    name = mkName str,
                    linkage = (llvmLinkageTypeToLinkage link)
                    visibility = Default,
                    isThreadLocal = False,
                    addrSpace = 0,
                    hasUnnamedAddr = False,
                    isConstant = (con == Constant),
                    type' = (llvmTypeToType ty),
                    initializer = Nothing,
                    section = sec >>= (Just . unpackFS),
                    alignment = if ali==Nothing then 0 else fromJust ali
                  }
llvmVarToGlobal (LMLocalVar uniq ty) = undefined
llvmVarToGlobal (LMNLocalVar str ty) = undefined
llvmVarToGlobal (LMLitVar lit) = undefined


floatToSomeFloat :: Double -> LlvmType -> SomeFloat
floatToSomeFloat d ty =
    case ty of
      LMFloat    -> Single d
      LMDouble   -> Double d
      -- X86_FP80 {- need to split into a 16 and 64 bit word -}
      LMFloat80  -> error "TypeConversions: X86 specific 80 bit floats not implemented."
      -- Quadruple {- need to split into two 64 bit words -}
      LMFloat128 -> error "TypeConversions: 128 bit floats not implemented."
      _          -> error (show ty) ++ " is not an floating type."

llvmTypeToType :: LlvmType -> Type
llvmTypeToType ty =
    case ty of
      LMInt width -> IntegerType width
      LMFloat -> FloatingPointType 32 IEEE
      LMDouble -> FloatingPointType 64 IEEE
      LMFloat80 -> FloatingPointType 80 DoubleExtended
      LMFloat128 -> FloatingPointType 128 IEEE
      LMPointer ty -> PointerType (llvmTypeToType ty) (AddrSpace 0) -- default address space
      LMArray len ty -> ArrayType len (llvmTypeToType ty)
      LMVector len typ -> VectorType len (llvmTypeToType ty)
      LMLabel -> undefined
      LMVoid -> VoidType
      LMStruct tys -> StructureType false (map llvmTypeToType tys) -- not packed
      LMAlias ali -> NamedTypeReference
      LMMetadata -> MetaDataType
      LMFunction decl@(name link cc ty vArgs params ali) -> FunctionType (llvmTypeToType ty) (map (llvmTypeToType . fst) params) (vArgs == VarArgs)

llvmStaticToConstant :: LlvmStatic -> Constant
llvmStaticToConstant stat =
    case stat of
      LMComment str -> undefined
      LMStaticLit lit -> 
          case lit of
            LMIntLit i width -> Int width i
            LMFloatLit d ty  -> Float (floatToSomeFloat d ty)
            LMNullLit ty     -> Null (llvmTypeToType ty)
            LMVectorLit lits -> Vector (map (llvmStaticToConstant . LlvmStaticLit) lits)
            LMUndefLit       -> Undef VoidType
      LMUninitType ty -> Undef (llvmTypeToType ty)
      LMStaticStr str ty -> 
      LMStaticArray stats ty -> Array (llvmTypeToType ty) (map llvmStaticToConstant stats)
      LMStaticStruc stats ty -> Vector (map llvmStaticToConstant stats)
      LMStaticPointer var -> undefined
      -- static expressions
      LMBitc stat ty -> BitCast (llvmStaticToConstant stat) (llvmTypeToType ty)
      LMPtoI stat ty -> IntToPtr (llvmStaticToConstant stat) (llvmTypeToType ty)
      LMAdd statL statR -> Add False False (llvmStaticToConstant statL) (llvmStaticToConstant statR) -- bools are for no (un)signed wrap
      LMSub statL statR -> Sub False False (llvmStaticToConstant statL) (llvmStaticToConstant statR) -- bools are for no (un)signed wrap

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

llvmCmpOpToPredicate :: LlvmCmpOp -> Either IntegerPredicate FloatingPointPredicate
llvmCmpOpToPredicate op =
    let intOp = llvmCmpOpToIntegerPredicate op
        fpOp  = llvmCmpOpToFloatingPointPredicate op
    in if intOp /= Nothing then Left (fromJust intOp) else Right (fromJust fpOp)

-- Convert comparator operators to integer predicates
llvmCmpOpToIntegerPredicate :: LlvmCmpOp -> Maybe IntegerPredicate
llvmCmpOpToIntegerPredicate op =
    case op of
      LM_CMP_Eq  -> Just EQ
      LM_CMP_Ne  -> Just NE
      LM_CMP_Ugt -> Just UGT
      LM_CMP_Uge -> Just UGE
      LM_CMP_Ult -> Just ULT
      LM_CMP_Ule -> Just ULE
      LM_CMP_Sgt -> Just SGT
      LM_CMP_Sge -> Just SGE
      LM_CMP_Slt -> Just SLT
      LM_CMP_Sle -> Just SLE
      _          -> Nothing

-- The difference between O and U prefixed predicates relates to qNaN (quiet NaN) values
llvmCmpOpToFloatingPointPredicate :: LlvmCmpOp -> FloatingPointPredicate
llvmCmpOpToFloatingPointPredicate op =
    case op of
      LM_CMP_Feq -> Just OEQ
      LM_CMP_Fne -> Just ONE
      LM_CMP_Fgt -> Just OGT
      LM_CMP_Fge -> Just OGE
      LM_CMP_Flt -> Just OLT
      LM_CMP_Fle -> Just OLE
      _          -> Nothing


llvmVarToOperand :: LlvmVar -> Operand
llvmVarToOperand (LMGlobalVar str ty link sec ali con) = ConstantOperand (GlobalReference (mkName str))
-- N.B: Hashing a Unique technically doesn't guarantee a unique Int.
--      However, uniques are generated by the sequence Unique 1, Unique 2, ...
--      Thus we won't get any collisions until we call newUnique 2^32 or 2^64 times.
llvmVarToOperand (LMLocalVar uniq ty) = LocalReference (UnName (hashUnique uniq))
llvmVarToOperand (LMNLocalVar str ty) = LocalReference (mkName str)
llvmVarToOperand (LMLitVar lit) = ConstantOperand (llvmStaticToConstant (LMStaticLit lit))

llvmParameterToNamedParameter :: LlvmParameter -> Either String Word -> Parameter
llvmParameterToNamedParameter (ty, attrs) name =
    Parameter ty' name' attrs'
        where attrs' = map llvmParamAttrToParameterAttribute attrs
              ty' = llvmTypeToType ty
              name' = either Name UnName name

-- Can we get rid of the IO here?
llvmParameterToParameter :: LlvmParameter -> IO Parameter
llvmParameterToParameter param =
    do name <- newUnique
       llvmParameterToNamedParameter param (hashUnique name)

platformToDataLayout :: Platform -> DataLayout
platformToDataLayout platform =
    case platform of
      Platform { platformArch = ArchX86, platformOS = OSDarwin } ->
          DataLayout { endianness = Just LittleEndian,
                       stackAlignment = Nothing, -- default stack alignment
                       pointerLayouts = Map.fromList [(AddrSpace 0, (32, AlignmentInfo 32 32))],
                       typeLayouts = Map.fromList [((IntegerAlign, 1), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 8), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 16), AlignmentInfo 16 16),
                                                   ((IntegerAlign, 32), AlignmentInfo 32 32),
                                                   ((IntegerAlign, 64), AlignmentInfo 32 64),
                                                   ((FloatAlign, 32), AlignmentInfo 32 32),
                                                   ((FloatAlign, 64), AlignmentInfo 32 64),
                                                   ((VectorAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 128), AlignmentInfo 128 128),
                                                   ((AggregateAlign, 0), AlignmentInfo 0 64),
                                                   ((FloatAlign, 80), AlignmentInfo 128 128)]
                       nativeSizes = Just (Set.FromList [8, 16, 32])
                     }
      Platform { platformArch = ArchX86, platformOS = OSMinGW32 } ->
          DataLayout { endianness = Just LittleEndian,
                       stackAlignment = Nothing, -- default stack alignment
                       pointerLayouts = Map.fromList [(AddrSpace 0, (32, AlignmentInfo 32 32))],
                       typeLayouts = Map.fromList [((IntegerAlign, 1), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 8), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 16), AlignmentInfo 16 16),
                                                   ((IntegerAlign, 32), AlignmentInfo 32 32),
                                                   ((IntegerAlign, 64), AlignmentInfo 64 64),
                                                   ((FloatAlign, 32), AlignmentInfo 32 32),
                                                   ((FloatAlign, 64), AlignmentInfo 32 64),
                                                   ((VectorAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 128), AlignmentInfo 128 128),
                                                   ((AggregateAlign, 0), AlignmentInfo 0 64)],
                                                   --n.b. original data layout (erroneously?) had 2 values for f64, 128:128 and 32:32. Going with 32:32 for now.
                                                   ((FloatAlign, 80), AlignmentInfo 32 32)],
                       nativeSizes = Just (Set.FromList [8, 16, 32])
                     }
      Platform { platformArch = ArchX86, platformOS = OSLinux } ->
          DataLayout { endianness = Just LittleEndian,
                       stackAlignment = Nothing, -- default stack alignment
                       pointerLayouts = Map.fromList [(AddrSpace 0, (32, AlignmentInfo 32 32))],
                       typeLayouts = Map.fromList [((IntegerAlign, 1), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 8), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 16), AlignmentInfo 16 16),
                                                   ((IntegerAlign, 32), AlignmentInfo 32 32),
                                                   ((IntegerAlign, 64), AlignmentInfo 32 64),
                                                   ((FloatAlign, 32), AlignmentInfo 32 32),
                                                   ((FloatAlign, 64), AlignmentInfo 32 64),
                                                   ((VectorAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 128), AlignmentInfo 128 128),
                                                   ((AggregateAlign, 0), AlignmentInfo 0 64),
                                                   ((FloatAlign, 80), AlignmentInfo 32 32)],
                       nativeSizes = Just (Set.FromList [8, 16, 32])
                     }
      Platform { platformArch = ArchX86_64, platformOS = OSDarwin } ->
          DataLayout { endianness = Just LittleEndian,
                       stackAlignment = Nothing, -- default stack alignment
                       pointerLayouts = Map.fromList [(AddrSpace 0, (64, AlignmentInfo 64 64))],
                       typeLayouts = Map.fromList [((IntegerAlign, 1), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 8), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 16), AlignmentInfo 16 16),
                                                   ((IntegerAlign, 32), AlignmentInfo 32 32),
                                                   ((IntegerAlign, 64), AlignmentInfo 64 64),
                                                   ((FloatAlign, 32), AlignmentInfo 32 32),
                                                   ((FloatAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 128), AlignmentInfo 128 128),
                                                   ((AggregateAlign, 0), AlignmentInfo 0 64),
                                                   ((StackAlign, 0), AlignmentInfo 64 64),
                                                   ((FloatAlign, 80), AlignmentInfo 128 128)],
                       nativeSizes = Just (Set.FromList [8, 16, 32, 64])
                     }
      Platform { platformArch = ArchX86_64, platformOS = OSLinux } ->
          DataLayout { endianness = Just LittleEndian,
                       stackAlignment = Nothing, -- default stack alignment
                       pointerLayouts = Map.fromList [(AddrSpace 0, (64, AlignmentInfo 64 64))],
                       typeLayouts = Map.fromList [((IntegerAlign, 1), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 8), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 16), AlignmentInfo 16 16),
                                                   ((IntegerAlign, 32), AlignmentInfo 32 32),
                                                   ((IntegerAlign, 64), AlignmentInfo 64 64),
                                                   ((FloatAlign, 32), AlignmentInfo 32 32),
                                                   ((FloatAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 128), AlignmentInfo 128 128),
                                                   ((AggregateAlign, 0), AlignmentInfo 0 64),
                                                   ((StackAlign, 0), AlignmentInfo 64 64),
                                                   ((FloatAlign, 80), AlignmentInfo 128 128)],
                       nativeSizes = Just (Set.FromList [8, 16, 32, 64])
                     }
      Platform { platformArch = ArchARM {}, platformOS = OSLinux } ->
          DataLayout { endianness = Just LittleEndian,
                       stackAlignment = Nothing, -- default stack alignment
                       pointerLayouts = Map.fromList [(AddrSpace 0, (32, AlignmentInfo 32 32))],
                       typeLayouts = Map.fromList [((IntegerAlign, 1), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 8), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 16), AlignmentInfo 16 16),
                                                   ((IntegerAlign, 32), AlignmentInfo 32 32),
                                                   ((IntegerAlign, 64), AlignmentInfo 64 64),
                                                   ((FloatAlign, 32), AlignmentInfo 32 32),
                                                   ((FloatAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 128), AlignmentInfo 64 128),
                                                   ((AggregateAlign, 0), AlignmentInfo 0 64)]
                       nativeSizes = Just (Set.FromList [32])
                     }
      Platform { platformArch = ArchARM {}, platformOS = OSAndroid } ->
          DataLayout { endianness = Just LittleEndian,
                       stackAlignment = Nothing, -- default stack alignment
                       pointerLayouts = Map.fromList [(AddrSpace 0, (32, AlignmentInfo 32 32))],
                       typeLayouts = Map.fromList [((IntegerAlign, 1), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 8), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 16), AlignmentInfo 16 16),
                                                   ((IntegerAlign, 32), AlignmentInfo 32 32),
                                                   ((IntegerAlign, 64), AlignmentInfo 64 64),
                                                   ((FloatAlign, 32), AlignmentInfo 32 32),
                                                   ((FloatAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 128), AlignmentInfo 64 128),
                                                   ((AggregateAlign, 0), AlignmentInfo 0 64)]
                       nativeSizes = Just (Set.FromList [32])
                     }
      Platform { platformArch = ArchARM {}, platformOS = OSQNXNTO } ->
          DataLayout { endianness = Just LittleEndian,
                       stackAlignment = Nothing, -- default stack alignment
                       pointerLayouts = Map.fromList [(AddrSpace 0, (32, AlignmentInfo 32 32))],
                       typeLayouts = Map.fromList [((IntegerAlign, 1), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 8), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 16), AlignmentInfo 16 16),
                                                   ((IntegerAlign, 32), AlignmentInfo 32 32),
                                                   ((IntegerAlign, 64), AlignmentInfo 64 64),
                                                   ((FloatAlign, 32), AlignmentInfo 32 32),
                                                   ((FloatAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 128), AlignmentInfo 64 128),
                                                   ((AggregateAlign, 0), AlignmentInfo 0 64)]
                       nativeSizes = Just (Set.FromList [32])
                     }
      Platform { platformArch = ArchARM {}, platformOS = OSiOS } ->
          DataLayout { endianness = Just LittleEndian,
                       stackAlignment = Nothing, -- default stack alignment
                       pointerLayouts = Map.fromList [(AddrSpace 0, (32, AlignmentInfo 32 32))],
                       typeLayouts = Map.fromList [((IntegerAlign, 1), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 8), AlignmentInfo 8 8),
                                                   ((IntegerAlign, 16), AlignmentInfo 16 16),
                                                   ((IntegerAlign, 32), AlignmentInfo 32 32),
                                                   ((IntegerAlign, 64), AlignmentInfo 64 64),
                                                   ((FloatAlign, 32), AlignmentInfo 32 32),
                                                   ((FloatAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 64), AlignmentInfo 64 64),
                                                   ((VectorAlign, 128), AlignmentInfo 64 128),
                                                   ((AggregateAlign, 0), AlignmentInfo 0 64)]
                       nativeSizes = Just (Set.FromList [32])
                     }
      _ ->
          defaultDataLayout

platformToTargetTriple :: Platform -> String
    case platform of
    Platform { platformArch = ArchX86, platformOS = OSDarwin } ->
        "i386-apple-darwin9.8"
    Platform { platformArch = ArchX86, platformOS = OSMinGW32 } ->
        "i686-pc-win32"
    Platform { platformArch = ArchX86, platformOS = OSLinux } ->
        "i386-pc-linux-gnu"
    Platform { platformArch = ArchX86_64, platformOS = OSDarwin } ->
        "x86_64-apple-darwin10.0.0"
    Platform { platformArch = ArchX86_64, platformOS = OSLinux } ->
        "x86_64-linux-gnu"
    Platform { platformArch = ArchARM {}, platformOS = OSLinux } ->
        "arm-unknown-linux-gnueabi"
    Platform { platformArch = ArchARM {}, platformOS = OSAndroid } ->
        "arm-unknown-linux-androideabi"
    Platform { platformArch = ArchARM {}, platformOS = OSQNXNTO } ->
        "arm-unknown-nto-qnx8.0.0eabi"
    Platform { platformArch = ArchARM {}, platformOS = OSiOS } ->
        "arm-apple-darwin10"
    _ ->
        ""

llvmVarToName :: LlvmVar -> Name
llvmVarToName (LMGlobalVar name ty link sec ali con) = Name name
llvmVarToName (LMLocalVar uniq ty) = UnName uniq
llvmVarToName (LMNLocalVar name ty) = Name name
llvmVarToName _ = error "llvmVarToName: not a valid name"

llvmVarToConstant :: LlvmVar -> Constant
llvmVarToConstant v@(LMGlobalVar name ty link sec ali con) = GlobalReference (llvmVarToName v)
llvmVarToConstant v@(LMLocalVar uniq ty) = 
llvmVarToConstant v@(LMNLocalVar str ty) = undefined
llvmVarToConstant v@(LMLitVar lit) = undefined

mkName :: LMString -> Name
mkName = Name . unpackFS

metaExprToMetadataNode :: MetaExpr -> MetadataNode
metaExprToMetadataNode (MetaStr    s ) =
    MetadataNode [MetadataStringOperand (unpackFS s)]
metaExprToMetadataNode (MetaNode   n ) =
    MetadataNodeReference (MetadataNodeID n)
metaExprToMetadataNode (MetaVar    v ) =
    case v of
      LMGlobalVar name LMMetadata link sec ali con ->
          MetadataNode [Just (ConstantOperand (llvmVarToConstant v))]
      LMLocalVar uniq LMMetadata ->
          MetadataNode [Just (LocalReference (llvmVarToName v))]
      LMNLocalVar str LMMetadata ->
          MetadataNode [Just (LocalReference (llvmVarToName v))]
      _ -> error "metaExprToMetadataNode: variable is not of type LMMetadata"
metaExprToMetadataNode (MetaStruct es) =
    MetadataNode $ map (Just . outputLlvmMetaExpr) es

llvmLitToConstant :: LlvmLit -> Constant
llvmLitToConstant lit =
    case lit of
      LMIntLit i ty -> Int (llvmWidthInBits dFlags ty) i
      LMFloatLit d ty -> Float (floatToSomeFloat d ty)
      LMNullLit ty -> Null (llvmTypeToType ty)
      LMVectorLit lits -> Vector (map llvmLitToConstant lits)
      LMUndefLit ty -> Undef (llvmTypeToType ty)

llvmExpressionToConstant :: LlvmExpression -> Constant
llvmExpressionToConstant expr =
    case expr of
      Alloca     tp amount          -> undefined
      LlvmOp     op left right      -> llvmOpToConstant op left right
      Call       tp fp args attrs   -> undefined
      CallM      tp fp args attrs   -> undefined
      Cast       LM_Bitcast from to -> BitCast (llvmVarToConstant from) (llvmTypeToType to)
      Cast       _ from to          -> undefined
      Compare    op left right      -> llvmCompareToConstant op left right
      Extract    vec idx            -> llvmExtractToConstant vec idx
      Insert     vec elt idx        -> llvmInsertToConstant vec elt idx
      GetElemPtr inb ptr indexes    -> llvmGetElemPtrToConstant inb ptr indexes
      Load       ptr                -> undefined
      Malloc     tp amount          -> undefined
      Phi        tp precessors      -> undefined
      Asm        asm c ty v se sk   -> undefined
      MExpr      meta e             -> undefined

llvmCompareToConstant :: LlvmCmpOp -> LlvmVar -> LlvmVar -> Constant
llvmCompareToConstant op left right =
    case op' of
      Right iOp -> ICmp iOp l r
      Left fpOp -> FCmp fpOp l r
    where op' = llvmCmpOpToPredicate op
          l = llvmVarToConstant left
          r = llvmVarToConstant right

llvmExtractToConstant :: LlvmVar -> LlvmVar -> Constant
llvmExtractToConstant vec idx =
    ExtractElement (llvmVarToConstant vec) (llvmVarToConstant idx)

llvmInsertToConstant :: LlvmVar -> LlvmVar -> LlvmVar -> Constant
llvmInsertToConstant vec elt idx =
    InsertElement (llvmVarToConstant vec) (llvmVarToConstant elt) (llvmVarToConstant idx)

llvmGetElemPtrToConstant :: Bool -> LlvmVar -> [LlvmVar] -> Constant
llvmGetElemPtrToConstant inb ptr indexes =
    GetElementPtr inb (llvmVarToConstant ptr) (map llvmVarToConstant indexes)

llvmOpToConstant :: LlvmMachOp -> LlvmVar -> LlvmVar -> Constant
llvmOpToConstant op left right =
    case op of
       (LM_MO_Add  -> Add False False
        LM_MO_Sub  -> Sub False False
        LM_MO_Mul  -> Mul False False
        LM_MO_UDiv -> UDiv False
        LM_MO_SDiv -> SDiv False
        LM_MO_URem -> URem
        LM_MO_SRem -> SRem
        LM_MO_FAdd -> FAdd
        LM_MO_FSub -> FSub
        LM_MO_FMul -> FMul
        LM_MO_FDiv -> FDiv
        LM_MO_FRem -> FRem
        LM_MO_Shl  -> Shl False False
        LM_MO_LShr -> LShr False
        LM_MO_AShr -> AShr False
        LM_MO_And  -> And
        LM_MO_Or   -> Or
        LM_MO_Xor  -> Xor) $ left' right'
           where left' = llvmVarToConstant left
                 right' = llvmVarToConstant right