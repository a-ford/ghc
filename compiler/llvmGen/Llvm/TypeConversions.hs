--------------------------------------------------------------------------------
-- | Conversions from backend types to llvm-general AST types
--

module Llvm.TypeConversions where

import Llvm.AbsSyn as AbsSyn
import Llvm.MetaData
import Llvm.Types as Types

import LLVM.General.AST as AST
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.AddrSpace as AS
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.FloatingPointPredicate as FPP
import qualified LLVM.General.AST.Operand as O
import qualified LLVM.General.AST.DataLayout as DL

import FastString
import Platform

import Unique
import UniqSupply
import Data.Word
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import GHC.Float
-- Many of these functions look kind of like a non-polymorphic id

llvmLinkageTypeToLinkage :: LlvmLinkageType -> L.Linkage
llvmLinkageTypeToLinkage link =
    case link of
      Types.Internal -> L.Internal
      Types.LinkOnce -> L.LinkOnce
      Types.Weak -> L.Weak
      Types.Appending -> L.Appending
      Types.ExternWeak -> L.ExternWeak
      Types.ExternallyVisible -> L.AvailableExternally
      Types.External -> L.External
      Types.Private -> L.Private

llvmVarToGlobal :: LlvmVar -> G.Global
llvmVarToGlobal (LMGlobalVar str ty link sec ali con) =
    G.GlobalVariable {
           G.name = mkName str,
           G.linkage = (llvmLinkageTypeToLinkage link),
           G.visibility = V.Default,
           G.isThreadLocal = False,
           G.addrSpace = AS.AddrSpace 0,
           G.hasUnnamedAddr = False,
           G.isConstant = (con == Constant),
           G.type' = (llvmTypeToType ty),
           G.initializer = Nothing,
           G.section = (Just . unpackFS) =<< sec,
           G.alignment = if ali==Nothing then 0 else (fromIntegral . fromJust) ali
         }
llvmVarToGlobal (LMLocalVar uniq ty) = undefined
llvmVarToGlobal (LMNLocalVar str ty) = undefined
llvmVarToGlobal (LMLitVar lit) = undefined


floatToSomeFloat :: Double -> LlvmType -> F.SomeFloat
floatToSomeFloat d ty =
    case ty of
      Types.LMFloat    -> F.Single (double2Float d)
      Types.LMDouble   -> F.Double d
      -- X86_FP80 {- need to split into a 16 and 64 bit word -}
      Types.LMFloat80  -> error "TypeConversions: X86 specific 80 bit floats not implemented."
      -- Quadruple {- need to split into two 64 bit words -}
      Types.LMFloat128 -> error "TypeConversions: 128 bit floats not implemented."
      _          -> error "Not an floating type." -- More specific error here please

llvmTypeToType :: LlvmType -> AST.Type
llvmTypeToType ty =
    case ty of
      Types.LMInt width -> IntegerType (fromIntegral width)
      Types.LMFloat -> FloatingPointType 32 IEEE
      Types.LMDouble -> FloatingPointType 64 IEEE
      Types.LMFloat80 -> FloatingPointType 80 DoubleExtended
      Types.LMFloat128 -> FloatingPointType 128 IEEE
      Types.LMPointer ty -> PointerType (llvmTypeToType ty) (AS.AddrSpace 0) -- don't know about address space
      Types.LMArray len ty -> ArrayType (fromIntegral len) (llvmTypeToType ty)
      Types.LMVector len typ -> VectorType (fromIntegral len) (llvmTypeToType ty)
      Types.LMLabel -> undefined
      Types.LMVoid -> VoidType
      Types.LMStruct tys -> StructureType False (map llvmTypeToType tys) -- not packed
      Types.LMAlias ali -> NamedTypeReference (Name ((unpackFS . fst) ali))
      Types.LMMetadata -> MetadataType
      Types.LMFunction decl@(LlvmFunctionDecl name link cc ty vArgs params ali) -> FunctionType (llvmTypeToType ty) (map (llvmTypeToType . fst) params) (vArgs == VarArgs)

llvmStaticToConstant :: LlvmStatic -> C.Constant
llvmStaticToConstant stat =
    case stat of
      Types.LMComment str -> undefined
      Types.LMStaticLit lit ->
          case lit of
            Types.LMIntLit i width -> C.Int (fromIntegral (llvmIntWidth width)) i
            Types.LMFloatLit d ty  -> C.Float (floatToSomeFloat d ty)
            Types.LMNullLit ty     -> C.Null (llvmTypeToType ty)
            Types.LMVectorLit lits -> C.Vector (map (llvmStaticToConstant . LMStaticLit) lits)
            Types.LMUndefLit ty    -> C.Undef (llvmTypeToType ty)
      Types.LMUninitType ty -> C.Undef (llvmTypeToType ty)
      Types.LMStaticStr str ty -> undefined -- FIXME: Couldn't find an appropriate mapping
      Types.LMStaticArray stats ty -> C.Array (llvmTypeToType ty) (map llvmStaticToConstant stats)
      Types.LMStaticStruc stats ty -> C.Vector (map llvmStaticToConstant stats)
      Types.LMStaticPointer var -> undefined
      -- static expressions
      Types.LMBitc stat ty -> C.BitCast (llvmStaticToConstant stat) (llvmTypeToType ty)
      Types.LMPtoI stat ty -> C.IntToPtr (llvmStaticToConstant stat) (llvmTypeToType ty)
      Types.LMAdd statL statR -> C.Add False False (llvmStaticToConstant statL) (llvmStaticToConstant statR) -- bools are for no (un)signed wrap
      Types.LMSub statL statR -> C.Sub False False (llvmStaticToConstant statL) (llvmStaticToConstant statR) -- bools are for no (un)signed wrap

llvmCallConventionToCallingConvention :: LlvmCallConvention -> CC.CallingConvention
llvmCallConventionToCallingConvention conv =
    case conv of
      CC_Ccc -> CC.C
      CC_Fastcc -> CC.Fast
      CC_Coldcc -> CC.Cold
      CC_Ncc 10 -> CC.GHC
      CC_X86_Stdcc -> CC.Numbered 64
      CC_Ncc code -> CC.Numbered (fromIntegral code)

llvmFuncAttrToFunctionAttribute :: LlvmFuncAttr -> A.FunctionAttribute
llvmFuncAttrToFunctionAttribute attr =
    case attr of
      AlwaysInline -> A.AlwaysInline
      InlineHint -> A.InlineHint
      NoInline -> A.NoInline
      OptSize -> A.OptimizeForSize
      NoReturn -> A.NoReturn
      NoUnwind -> A.NoUnwind
      ReadNone -> A.ReadNone
      ReadOnly -> A.ReadOnly
      Ssp -> A.StackProtect
      SspReq -> A.StackProtectReq
      NoRedZone -> A.NoRedZone
      NoImplicitFloat -> A.NoImplicitFloat
      Naked -> A.Naked

llvmParamAttrToParameterAttribute :: LlvmParamAttr -> A.ParameterAttribute
llvmParamAttrToParameterAttribute attr =
    case attr of
      ZeroExt -> A.ZeroExt
      SignExt -> A.SignExt
      InReg -> A.InReg
      ByVal -> A.ByVal
      SRet -> A.SRet
      NoAlias -> A.NoAlias
      NoCapture -> A.NoCapture
      Nest -> A.Nest

llvmCmpOpToPredicate :: LlvmCmpOp -> Either IP.IntegerPredicate FPP.FloatingPointPredicate
llvmCmpOpToPredicate op =
    let intOp = llvmCmpOpToIntegerPredicate op
        fpOp  = llvmCmpOpToFloatingPointPredicate op
    in if intOp /= Nothing then Left (fromJust intOp) else Right (fromJust fpOp)

-- Convert comparator operators to integer predicates
llvmCmpOpToIntegerPredicate :: LlvmCmpOp -> Maybe IP.IntegerPredicate
llvmCmpOpToIntegerPredicate op =
    case op of
      LM_CMP_Eq  -> Just IP.EQ
      LM_CMP_Ne  -> Just IP.NE
      LM_CMP_Ugt -> Just IP.UGT
      LM_CMP_Uge -> Just IP.UGE
      LM_CMP_Ult -> Just IP.ULT
      LM_CMP_Ule -> Just IP.ULE
      LM_CMP_Sgt -> Just IP.SGT
      LM_CMP_Sge -> Just IP.SGE
      LM_CMP_Slt -> Just IP.SLT
      LM_CMP_Sle -> Just IP.SLE
      _          -> Nothing

-- The difference between O and U prefixed predicates relates to qNaN (quiet NaN) values
llvmCmpOpToFloatingPointPredicate :: LlvmCmpOp -> Maybe FPP.FloatingPointPredicate
llvmCmpOpToFloatingPointPredicate op =
    case op of
      LM_CMP_Feq -> Just FPP.OEQ
      LM_CMP_Fne -> Just FPP.ONE
      LM_CMP_Fgt -> Just FPP.OGT
      LM_CMP_Fge -> Just FPP.OGE
      LM_CMP_Flt -> Just FPP.OLT
      LM_CMP_Fle -> Just FPP.OLE
      _          -> Nothing


llvmVarToOperand :: LlvmVar -> O.Operand
llvmVarToOperand (LMGlobalVar str ty link sec ali con) = ConstantOperand (C.GlobalReference (mkName str))
-- Hashing a Unique technically doesn't guarantee a unique Int, but we won't get
-- any collisions until 2^32 or 2^64 calls.
llvmVarToOperand (LMLocalVar uniq ty) = LocalReference ((UnName . fromIntegral . getKey) uniq)
llvmVarToOperand (LMNLocalVar str ty) = LocalReference (mkName str)
llvmVarToOperand (LMLitVar lit) = ConstantOperand (llvmStaticToConstant (LMStaticLit lit))

llvmParameterToNamedParameter :: LlvmParameter -> Either String Word -> AST.Parameter
llvmParameterToNamedParameter (ty, attrs) name =
    Parameter ty' name' attrs'
        where attrs' = map llvmParamAttrToParameterAttribute attrs
              ty' = llvmTypeToType ty
              name' = either Name UnName name

-- Can we get rid of the IO here? (not without more context to assign unique names)
llvmParameterToParameter :: LlvmParameter -> IO AST.Parameter
llvmParameterToParameter param =
    do us <- mkSplitUniqSupply 'l'
       let name = uniqFromSupply us
       return (llvmParameterToNamedParameter param (Right (fromIntegral (getKey name))))

platformToDataLayout :: Platform -> DL.DataLayout
platformToDataLayout platform =
    case platform of
      Platform { platformArch = ArchX86, platformOS = OSDarwin } ->
          DL.DataLayout { DL.endianness = Just DL.LittleEndian,
                          DL.stackAlignment = Nothing, -- default stack alignment
                          DL.pointerLayouts = Map.fromList [(AS.AddrSpace 0,
                                                             (32, DL.AlignmentInfo 32 (Just 32)))],
                          DL.typeLayouts = Map.fromList [((DL.IntegerAlign, 1),
                                                          DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 8),
                                                    DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 16),
                                                    DL.AlignmentInfo 16 (Just 16)),
                                                   ((DL.IntegerAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.IntegerAlign, 64),
                                                    DL.AlignmentInfo 32 (Just 64)),
                                                   ((DL.FloatAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.FloatAlign, 64),
                                                    DL.AlignmentInfo 32 (Just 64)),
                                                   ((DL.VectorAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 128),
                                                    DL.AlignmentInfo 128 (Just 128)),
                                                   ((DL.AggregateAlign, 0),
                                                    DL.AlignmentInfo 0 (Just 64)),
                                                   ((DL.FloatAlign, 80),
                                                    DL.AlignmentInfo 128 (Just 128))],
                       DL.nativeSizes = Just (Set.fromList [8, 16, 32])
                     }
      Platform { platformArch = ArchX86, platformOS = OSMinGW32 } ->
          DL.DataLayout { DL.endianness = Just DL.LittleEndian,
                       DL.stackAlignment = Nothing, -- default stack alignment
                       DL.pointerLayouts = Map.fromList [(AS.AddrSpace 0, 
                                                          (32, DL.AlignmentInfo 32 (Just 32)))],
                       DL.typeLayouts = Map.fromList [((DL.IntegerAlign, 1),
                                                       DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 8),
                                                    DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 16),
                                                    DL.AlignmentInfo 16 (Just 16)),
                                                   ((DL.IntegerAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.IntegerAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.FloatAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.FloatAlign, 64),
                                                    DL.AlignmentInfo 32 (Just 64)),
                                                   ((DL.VectorAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 128),
                                                    DL.AlignmentInfo 128 (Just 128)),
                                                   ((DL.AggregateAlign, 0),
                                                    DL.AlignmentInfo 0 (Just 64)),
                                                   --n.b. original data layout (erroneously?) had 2 values for f64, 
                                                   -- 128:128 and 32:32. Going with 32:32 for now.
                                                   ((DL.FloatAlign, 80),
                                                    DL.AlignmentInfo 32 (Just 32))],
                       DL.nativeSizes = Just (Set.fromList [8, 16, 32])
                     }
      Platform { platformArch = ArchX86, platformOS = OSLinux } ->
          DL.DataLayout { DL.endianness = Just DL.LittleEndian,
                       DL.stackAlignment = Nothing, -- default stack alignment
                       DL.pointerLayouts = Map.fromList [(AS.AddrSpace 0,
                                                          (32, DL.AlignmentInfo 32 (Just 32)))],
                       DL.typeLayouts = Map.fromList [((DL.IntegerAlign, 1),
                                                       DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 8),
                                                    DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 16),
                                                    DL.AlignmentInfo 16 (Just 16)),
                                                   ((DL.IntegerAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.IntegerAlign, 64),
                                                    DL.AlignmentInfo 32 (Just 64)),
                                                   ((DL.FloatAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.FloatAlign, 64),
                                                    DL.AlignmentInfo 32 (Just 64)),
                                                   ((DL.VectorAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 128),
                                                    DL.AlignmentInfo 128 (Just 128)),
                                                   ((DL.AggregateAlign, 0),
                                                    DL.AlignmentInfo 0 (Just 64)),
                                                   ((DL.FloatAlign, 80),
                                                    DL.AlignmentInfo 32 (Just 32))],
                       DL.nativeSizes = Just (Set.fromList [8, 16, 32])
                     }
      Platform { platformArch = ArchX86_64, platformOS = OSDarwin } ->
          DL.DataLayout { DL.endianness = Just DL.LittleEndian,
                       DL.stackAlignment = Nothing, -- default stack alignment
                       DL.pointerLayouts = Map.fromList [(AS.AddrSpace 0, 
                                                          (64, DL.AlignmentInfo 64 (Just 64)))],
                       DL.typeLayouts = Map.fromList [((DL.IntegerAlign, 1),
                                                       DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 8),
                                                    DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 16),
                                                    DL.AlignmentInfo 16 (Just 16)),
                                                   ((DL.IntegerAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.IntegerAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.FloatAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.FloatAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 128),
                                                    DL.AlignmentInfo 128 (Just 128)),
                                                   ((DL.AggregateAlign, 0),
                                                    DL.AlignmentInfo 0 (Just 64)),
                                                   ((DL.StackAlign, 0),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.FloatAlign, 80),
                                                    DL.AlignmentInfo 128 (Just 128))],
                       DL.nativeSizes = Just (Set.fromList [8, 16, 32, 64])
                     }
      Platform { platformArch = ArchX86_64, platformOS = OSLinux } ->
          DL.DataLayout { DL.endianness = Just DL.LittleEndian,
                       DL.stackAlignment = Nothing, -- default stack alignment
                       DL.pointerLayouts = Map.fromList [(AS.AddrSpace 0,
                                                          (64, DL.AlignmentInfo 64 (Just 64)))],
                       DL.typeLayouts = Map.fromList [((DL.IntegerAlign, 1),
                                                       DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 8),
                                                    DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 16),
                                                    DL.AlignmentInfo 16 (Just 16)),
                                                   ((DL.IntegerAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.IntegerAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.FloatAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.FloatAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 128),
                                                    DL.AlignmentInfo 128 (Just 128)),
                                                   ((DL.AggregateAlign, 0),
                                                    DL.AlignmentInfo 0 (Just 64)),
                                                   ((DL.StackAlign, 0),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.FloatAlign, 80),
                                                    DL.AlignmentInfo 128 (Just 128))],
                       DL.nativeSizes = Just (Set.fromList [8, 16, 32, 64])
                     }
      Platform { platformArch = ArchARM {}, platformOS = OSLinux } ->
          DL.DataLayout { DL.endianness = Just DL.LittleEndian,
                       DL.stackAlignment = Nothing, -- default stack alignment
                       DL.pointerLayouts = Map.fromList [(AS.AddrSpace 0,
                                                          (32, DL.AlignmentInfo 32 (Just 32)))],
                       DL.typeLayouts = Map.fromList [((DL.IntegerAlign, 1),
                                                       DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 8),
                                                    DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 16),
                                                    DL.AlignmentInfo 16 (Just 16)),
                                                   ((DL.IntegerAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.IntegerAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.FloatAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.FloatAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 128),
                                                    DL.AlignmentInfo 64 (Just 128)),
                                                   ((DL.AggregateAlign, 0),
                                                    DL.AlignmentInfo 0 (Just 64))],
                       DL.nativeSizes = Just (Set.fromList [32])
                     }
      Platform { platformArch = ArchARM {}, platformOS = OSAndroid } ->
          DL.DataLayout { DL.endianness = Just DL.LittleEndian,
                       DL.stackAlignment = Nothing, -- default stack alignment
                       DL.pointerLayouts = Map.fromList [(AS.AddrSpace 0,
                                                          (32, DL.AlignmentInfo 32 (Just 32)))],
                       DL.typeLayouts = Map.fromList [((DL.IntegerAlign, 1),
                                                       DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 8),
                                                    DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 16),
                                                    DL.AlignmentInfo 16 (Just 16)),
                                                   ((DL.IntegerAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.IntegerAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.FloatAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.FloatAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 128),
                                                    DL.AlignmentInfo 64 (Just 128)),
                                                   ((DL.AggregateAlign, 0),
                                                    DL.AlignmentInfo 0 (Just 64))],
                       DL.nativeSizes = Just (Set.fromList [32])
                     }
      Platform { platformArch = ArchARM {}, platformOS = OSQNXNTO } ->
          DL.DataLayout { DL.endianness = Just DL.LittleEndian,
                       DL.stackAlignment = Nothing, -- default stack alignment
                       DL.pointerLayouts = Map.fromList [(AS.AddrSpace 0,
                                                          (32, DL.AlignmentInfo 32 (Just 32)))],
                       DL.typeLayouts = Map.fromList [((DL.IntegerAlign, 1),
                                                       DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 8),
                                                    DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 16),
                                                    DL.AlignmentInfo 16 (Just 16)),
                                                   ((DL.IntegerAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.IntegerAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.FloatAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.FloatAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 128),
                                                    DL.AlignmentInfo 64 (Just 128)),
                                                   ((DL.AggregateAlign, 0),
                                                    DL.AlignmentInfo 0 (Just 64))],
                       DL.nativeSizes = Just (Set.fromList [32])
                     }
      Platform { platformArch = ArchARM {}, platformOS = OSiOS } ->
          DL.DataLayout { DL.endianness = Just DL.LittleEndian,
                       DL.stackAlignment = Nothing, -- default stack alignment
                       DL.pointerLayouts = Map.fromList [(AS.AddrSpace 0,
                                                          (32, DL.AlignmentInfo 32 (Just 32)))],
                       DL.typeLayouts = Map.fromList [((DL.IntegerAlign, 1),
                                                       DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 8),
                                                    DL.AlignmentInfo 8 (Just 8)),
                                                   ((DL.IntegerAlign, 16),
                                                    DL.AlignmentInfo 16 (Just 16)),
                                                   ((DL.IntegerAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.IntegerAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.FloatAlign, 32),
                                                    DL.AlignmentInfo 32 (Just 32)),
                                                   ((DL.FloatAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 64),
                                                    DL.AlignmentInfo 64 (Just 64)),
                                                   ((DL.VectorAlign, 128),
                                                    DL.AlignmentInfo 64 (Just 128)),
                                                   ((DL.AggregateAlign, 0),
                                                    DL.AlignmentInfo 0 (Just 64))],
                       DL.nativeSizes = Just (Set.fromList [32])
                     }
      _ ->
          DL.defaultDataLayout

platformToTargetTriple :: Platform -> String
platformToTargetTriple platform =
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

llvmVarToName :: LlvmVar -> AST.Name
llvmVarToName (LMGlobalVar name ty link sec ali con) = Name (unpackFS name)
llvmVarToName (LMLocalVar uniq ty) = (UnName . fromIntegral . getKey) uniq
llvmVarToName (LMNLocalVar name ty) = Name (unpackFS name)
llvmVarToName _ = error "llvmVarToName: not a valid name"

llvmVarToConstant :: LlvmVar -> C.Constant
llvmVarToConstant v@(LMGlobalVar name ty link sec ali con) = C.GlobalReference (llvmVarToName v)
llvmVarToConstant v@(LMLocalVar uniq ty) = undefined -- FIXME
llvmVarToConstant v@(LMNLocalVar str ty) = undefined
llvmVarToConstant v@(LMLitVar lit) = undefined

mkName :: LMString -> AST.Name
mkName = Name . unpackFS

metaExprToMetadataNode :: MetaExpr -> AST.MetadataNode
metaExprToMetadataNode (MetaStr    s ) =
    MetadataNode [Just (MetadataStringOperand (unpackFS s))]
metaExprToMetadataNode (MetaNode   n ) =
    MetadataNodeReference (MetadataNodeID (fromIntegral n))
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
    MetadataNode $ map (Just . metaExprToOperand) es

llvmLitToConstant :: LlvmLit -> C.Constant
llvmLitToConstant lit =
    case lit of
      LMIntLit i ty -> C.Int (fromIntegral (llvmIntWidth ty)) i
      LMFloatLit d ty -> C.Float (floatToSomeFloat d ty)
      LMNullLit ty -> C.Null (llvmTypeToType ty)
      LMVectorLit lits -> C.Vector (map llvmLitToConstant lits)
      LMUndefLit ty -> C.Undef (llvmTypeToType ty)

llvmExpressionToConstant :: LlvmExpression -> C.Constant
llvmExpressionToConstant expr =
    case expr of
      AbsSyn.Alloca tp amount          -> undefined
      LlvmOp     op left right      -> llvmOpToConstant op left right
      AbsSyn.Call tp fp args attrs   -> undefined
      CallM      tp fp args attrs   -> undefined
      Cast       LM_Bitcast from to -> C.BitCast (llvmVarToConstant from) (llvmTypeToType to)
      Cast       _ from to          -> undefined
      Compare    op left right      -> llvmCompareToConstant op left right
      Extract    vec idx            -> llvmExtractToConstant vec idx
      Insert     vec elt idx        -> llvmInsertToConstant vec elt idx
      GetElemPtr inb ptr indexes    -> llvmGetElemPtrToConstant inb ptr indexes
      AbsSyn.Load ptr                -> undefined
      Malloc     tp amount          -> undefined
      AbsSyn.Phi tp precessors      -> undefined
      Asm        asm c ty v se sk   -> undefined
      MExpr      meta e             -> undefined

llvmCompareToConstant :: LlvmCmpOp -> LlvmVar -> LlvmVar -> C.Constant
llvmCompareToConstant op left right =
    case op' of
      Left iOp -> C.ICmp iOp l r
      Right fpOp -> C.FCmp fpOp l r
    where op' = llvmCmpOpToPredicate op
          l = llvmVarToConstant left
          r = llvmVarToConstant right

llvmExtractToConstant :: LlvmVar -> LlvmVar -> C.Constant
llvmExtractToConstant vec idx =
    C.ExtractElement (llvmVarToConstant vec) (llvmVarToConstant idx)

llvmInsertToConstant :: LlvmVar -> LlvmVar -> LlvmVar -> C.Constant
llvmInsertToConstant vec elt idx =
    C.InsertElement (llvmVarToConstant vec) (llvmVarToConstant elt) (llvmVarToConstant idx)

llvmGetElemPtrToConstant :: Bool -> LlvmVar -> [LlvmVar] -> C.Constant
llvmGetElemPtrToConstant inb ptr indexes =
    C.GetElementPtr inb (llvmVarToConstant ptr) (map llvmVarToConstant indexes)

llvmOpToConstant :: LlvmMachOp -> LlvmVar -> LlvmVar -> C.Constant
llvmOpToConstant op left right =
    let left' = llvmVarToConstant left
        right' = llvmVarToConstant right in
    case op of
       LM_MO_Add  -> C.Add False False left' right'
       LM_MO_Sub  -> C.Sub False False left' right'
       LM_MO_Mul  -> C.Mul False False left' right'
       LM_MO_UDiv -> C.UDiv False left' right'
       LM_MO_SDiv -> C.SDiv False left' right'
       LM_MO_URem -> C.URem left' right'
       LM_MO_SRem -> C.SRem left' right'
       LM_MO_FAdd -> C.FAdd left' right'
       LM_MO_FSub -> C.FSub left' right'
       LM_MO_FMul -> C.FMul left' right'
       LM_MO_FDiv -> C.FDiv left' right'
       LM_MO_FRem -> C.FRem left' right'
       LM_MO_Shl  -> C.Shl False False left' right'
       LM_MO_LShr -> C.LShr False left' right'
       LM_MO_AShr -> C.AShr False left' right'
       LM_MO_And  -> C.And left' right'
       LM_MO_Or   -> C.Or left' right'
       LM_MO_Xor  -> C.Xor left' right'

-- | Output an LLVM metadata value.
metaExprToOperand :: MetaExpr -> Operand
metaExprToOperand (MetaStr    s ) =
    MetadataStringOperand (unpackFS s)
metaExprToOperand (MetaNode   n ) =
    MetadataNodeOperand (MetadataNodeReference (MetadataNodeID (fromIntegral n)))
metaExprToOperand (MetaVar    v ) =
    case v of
      LMGlobalVar name LMMetadata link sec ali con ->
          ConstantOperand (llvmVarToConstant v)
      LMLocalVar uniq LMMetadata ->
           LocalReference (llvmVarToName v)
      LMNLocalVar str LMMetadata ->
          LocalReference (llvmVarToName v)
      _ -> error "metaExprToOperand: variable is not of type LMMetadata"
metaExprToOperand (MetaStruct es) =
    MetadataNodeOperand (MetadataNode $ map (Just . metaExprToOperand) es)


-- Returns the width in bits of an integer type.
llvmIntWidth :: LlvmType -> Int
llvmIntWidth (LMInt n) = n
llvmIntWidth _         = error "Must give an integer type."