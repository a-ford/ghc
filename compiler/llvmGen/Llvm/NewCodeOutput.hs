--------------------------------------------------------------------------------
-- | Call into the Haskell LLVM API to generate LLVM bitcode.
--

module Llvm.CodeOutput where

import Llvm.AbsSyn
import Llvm.MetaData
import Llvm.Types

import DynFlags
import Unique

-- metadata and global assignment are the main sticking points

--------------------------------------------------------------------------------
-- * Top Level Output functions
--------------------------------------------------------------------------------

-- | Output out a whole LLVM module.
outputLlvmModule :: LlvmModule -> DynFlags -> Module --done
outputLlvmModule (LlvmModule comments aliases meta globals decls funcs) dflags
    = Module {
        moduleName = "<module-name-here>",
        moduleDataLayout = platformToDataLayout (targetPlatform dflags)
        moduleTargetTriple = Just (platformToTriple (targetPlatform dflags))
        moduleDefinintions = concat [alis, metas, glos, decs funs]
      }
    where alis  = outputLlvmAliases aliases
          metas = outputLlvmMetas meta
          glos  = outputLlvmGlobals globals
          decs  = outputLlvmFunctionDecls decls
          funs  = outputLlvmFunctions funcs

-- | Output out a list of global mutable variable definitions
outputLlvmGlobals :: [LMGlobal] -> [Definition] --done
outputLlvmGlobals ls = map outputLlvmGlobal ls

-- | Output out a global mutable variable definition
outputLlvmGlobal :: LMGlobal -> Definition --done
outputLlvmGlobal (LMGlobal var@(LMGlobalVar name ty link x a c) dat) =
    GlobalDefinition
    (globalVariableDefaults {
       name = mkName str,
       linkage = (llvmLinkageTypeToLinkage link)
       isConstant = (con == Constant),
       type' = (llvmTypeToType ty),
       initializer = dat >>= llvmStaticToConstant,
       section = sec >>= (Just . unpackFS),
       alignment = if ali==Nothing then 0 else fromJust ali
    })
    where varGlobal = llvmVarToGlobal var
          const = dat >>= llvmStaticToConstant

outputLlvmGlobal (LMGlobal var val) = sdocWithDynFlags $ \dflags ->
  error $ "Non Global var output as global! "
          ++ showSDoc dflags (ppr var) ++ " " ++ showSDoc dflags (ppr val)

-- | Output out a list of LLVM type aliases.
outputLlvmAliases :: [LlvmAlias] -> [Definition]  --done
outputLlvmAliases alis = map outputLlvmAlias alis

-- | Output out an LLVM type alias.
outputLlvmAlias :: LlvmAlias -> Definition --done
outputLlvmAlias (name, ty) = TypeDefinition (mkName name) (Just (llvmTypeToType ty))

-- | Output out a list of LLVM metadata.
outputLlvmMetas :: [MetaDecl] -> [Definition] --done
outputLlvmMetas metas = map outputLlvmMeta metas

-- | Output out an LLVM metadata definition
outputLlvmMeta :: MetaDecl -> Defintion
outputLlvmMeta (MetaUnamed n m) = MetadataNodeDefinition (MetaNodeID n) [(Just (outputLlvmMetaExpr m))]
outputLlvmMeta (MetaNamed n m) = NamedMetadataDefinition (unpackFS n) (map MetaNodeID m)

-- | Output an LLVM metadata value.
outputLlvmMetaExpr :: MetaExpr -> Operand --done
outputLlvmMetaExpr (MetaStr    s ) =
    MetadataStringOperand (unpackFS s)
outputLlvmMetaExpr (MetaNode   n ) =
    MetadataNodeOperand (MetadataNodeReference (MetadataNodeID n))
outputLlvmMetaExpr (MetaVar    v ) =
    case v of
      LMGlobalVar name LMMetadata link sec ali con ->
          ConstantOperand (llvmVarToConstant v)
      LMLocalVar uniq LMMetadata ->
           LocalReference (llvmVarToName v)
      LMNLocalVar str LMMetadata ->
          LocalReference (llvmVarToName v)
      _ -> error "outputLlvmMetaExpr: variable is not of type LMMetadata"
outputLlvmMetaExpr (MetaStruct es) =
    MetadataNodeOperand (MetadataNode $ map (Just . outputLlvmMetaExpr) es)

-- | Output out a list of function definitions.
outputLlvmFunctions :: LlvmFunctions -> [Definition] --done
outputLlvmFunctions funcs = map outputLlvmFunction funcs

-- | Output out a function definition.
-- body = [LlvmBlock] = [LlvmBlock {LlvmBlockId [LlvmStatement]}]
outputLlvmFunction :: LlvmFunction -> Definition --done
outputLlvmFunction (LlvmFunction dec@(LLVMFunctionDecl name link cc retTy vArgs params ali)
                                 args attrs sec body)
    = GlobalDefinition functionDefaults {
        linkage = llvmLinkageToLinkage link,
        callingConvention = llvmCallConventionToCallingConvention cc,
        returnType = llvmTypeToType retTy,
        name = mkName name,
        parameters = (map llvmParameterToParameter params, vArgs==VarArgs),
        functionAttributes = map llvmFuncAttrToFunctionAttribute attrs,
        section = sec >>= unpackFS,
        alignment = ali',
        basicBlocks = outputLlvmBlocks body
      }
      where ali' = if ali==Nothing then 0 else fromJust ali

{-
-- | Output out a function defenition header.
--outputLlvmFunctionHeader :: LlvmFunctionDecl -> [LMString] -> 
outputLlvmFunctionHeader (LlvmFunctionDecl n l c r varg p a) args
  = undefined 
-}

-- | Output out a list of function declaration.
outputLlvmFunctionDecls :: LlvmFunctionDecls -> [Definition]
outputLlvmFunctionDecls decs = map outputLlvmFunctionDecl decs

-- | Output out a function declaration.
-- Declarations define the function type but don't define the actual body of
-- the function.
outputLlvmFunctionDecl :: LlvmFunctionDecl -> Definition
outputLlvmFunctionDecl dec@(LlvmFunctionDecl n l c r varg p a)
  = outputLlvmFunction (LlvmFunction dec [] [] Nothing [])

-- | Output out a list of LLVM blocks.
outputLlvmBlocks :: LlvmBlocks -> [BasicBlock] --done
outputLlvmBlocks blocks = map outputLlvmBlock blocks

-- | Output out an LLVM block.
-- It must be part of a function definition.
-- BasicBlocks need '[Named Instruction]' and 'Named Terminator' type args,
-- hence the 'Do's. Not sure here with 'Do' vs. 'Name :='.
outputLlvmBlock :: LlvmBlock -> BasicBlock --done
outputLlvmBlock (LlvmBlock blockId stmts) =
    BasicBlock name (map Do instrs) (Do terminator)
        where
          name = UnName (hashUnique blockId)
          (instrs, terminator) = partitionEithers (map outputLlvmStatement stmts)

{-  let isLabel (MkLabel _) = True
      isLabel _           = False
      (block, rest)       = break isLabel stmts
      outputRest = case rest of
                     (MkLabel id):xs -> outputLlvmBlock (LlvmBlock id xs)
                     _               -> ()
  in do mapM_ outputLlvmStatement block
        outputRest
-}

-- | Output out an LLVM block label.
--outputLlvmBlockLabel :: LlvmBlockId -> 
--outputLlvmBlockLabel id = undefined 

-- | Output an LLVM statement.
outputLlvmStatement :: LlvmStatement -> Named (Either Instruction Terminator) --done
outputLlvmStatement stmt =
  case stmt of
    MetaStmt    meta s        -> outputMetaStatement meta s
    _                         -> outputMetaStatement [] stmt

-- | Output an LLVM statement with metadata annotations.
-- | By making instructions and terminators named, we may be able to do assignments.
outputMetaStatement :: [MetaAnnot] -> LlvmStatement -> Named (Either Instruction Terminator) --done(?)
outputMetaStatement meta stmt =
    case stmt of
      Assignment  dst expr      -> outputAssignment dst expr meta           -- Instruction (Broken?)
      Fence       st ord        -> Left $ outputFence st ord meta           -- Instruction
      Branch      target        -> Right $ outputBranch target meta         -- Terminator
      BranchIf    cond ifT ifF  -> Right $ outputBranchIf cond ifT ifF meta -- Terminator
      Comment     comments      -> undefined                                -- No need(?)
      MkLabel     label         -> undefined
      Store       value ptr     -> Left $ outputStore value ptr meta        -- Instruction
      Switch      scrut def tgs -> Right $ outputSwitch scrut def tgs meta  -- Terminator
      Return      result        -> Right $ outputReturn result meta         -- Terminator
      Expr        expr          -> Left $ outputLlvmExpression expr meta    -- Instruction
      Unreachable               -> Right $ Unreachable meta                 -- Terminator
      Nop                       -> undefined
      MetaStmt    meta s        -> outputMetaStatement meta s

-- | Output an LLVM expression.
outputLlvmExpression :: LlvmExpression -> Named Instruction --done
outputLlvmExpression expr
  = case expr of
      MExpr      meta e           -> outputMetaExpr meta e
      _                           -> outputMetaExpr [] e

outputMetaExpr :: [MetaAnnot] -> LlvmExpression -> Named Instruction --done
outputMetaExpr meta expr =
    case expr of
      Alloca     tp amount        -> outputAlloca tp amount meta
      LlvmOp     op left right    -> outputMachOp op left right meta
      Call       tp fp args attrs -> outputCall tp fp (map MetaVar args) attr meta
      CallM      tp fp args attrs -> outputCall tp fp args attrs meta
      Cast       op from to       -> outputCast op from to meta
      Compare    op left right    -> outputCmpOp op left right meta
      Extract    vec idx          -> outputExtract vec idx meta
      Insert     vec elt idx      -> outputInsert vec elt idx meta
      GetElemPtr inb ptr indexes  -> outputGetElementPtr inb ptr indexes meta
      Load       ptr              -> outputLoad ptr meta
      Malloc     tp amount        -> outputMalloc tp amount meta
      Phi        tp precessors    -> outputPhi tp precessors meta
      Asm        asm c ty v se sk -> outputAsm asm c ty v se sk
      MExpr      meta e           -> outputMetaExpr meta e

--------------------------------------------------------------------------------
-- * Individual print functions
--------------------------------------------------------------------------------

-- | Should always be a function pointer. So a global var of function type
-- (since globals are always pointers) or a local var of pointer function type.
outputCall :: LlvmCallType -> LlvmVar -> [MetaExpr] -> [LlvmFuncAttr] -> [MetaAnnot] -> Named Instruction --done
outputCall ct (LMGlobalVar _ (LMFunction decl@(name _ cc _ vArgs params _)) _ _ _ _) args attrs metas =
    Do $ Call { isTailCall = (ct==TailCall),
                callingConvention = llvmCallConventionToCallingConvention cc,
                returnAttributes = [],
                function = Right (ConstantOperand (GlobalReference (mkName name))),
                arguments = zip args' pattrs,
                functionAttributes = map llvmFuncAttrToFunctionAttribute attrs,
                metadata = outputMetaAnnots metas
              }
    where pattrs = map (map llvmParamAttrToParameterAttribute . snd) params
          args' = map outputMetaExpr outputMachOp

LlvmMachOp :: args -> LlvmVar -> LlvmVar -> [MetaAnnot] -> Named Instruction --done
outputMachOp op left right metas = Do $
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
        LM_MO_Xor  -> Xor) $ left' right' metas'
           where left' = llvmVarToOperand left
                 right' = llvmVarToOperand right
                 metas' = outputMetaAnnots metas


outputCmpOp :: LlvmCmpOp -> LlvmVar -> LlvmVar -> [MetaAnnot] -> Named Instruction ---done
outputCmpOp op left right metas = Do $ ICmp iPred left' right' metas'
    where iPred = fromJust (llvmCmpOpToIntegerPredicate op)
          left' = llvmVarToOperand left
          right' = llvmVarToOperand right
          metas' = outputMetaAnnots metas

-- | Not completely sure what an assignment should be in the context of a SSA
-- | based representation, but this might work.
outputAssignment :: LlvmVar -> LlvmExpression -> [MetaAnnot] -> Named Instruction
outputAssignment var expr metas =
    (llvmVarToName var) := (outputLlvmExpression (MExpr expr metas))

{-    case var of
      LMGlobalVar name ty link sec ali con ->  := 
      LMLocalVar uniq ty -> undefined
      LMLocalVar LMNLocalVar name ty -> undefined
      _ -> error "outputAssignment: not a valid var to assign."
-}

outputSyncOrdering :: LlvmSyncOrdering -> MemoryOrdering --done
outputSyncOrdering SyncUnord     = Unordered
outputSyncOrdering SyncMonotonic = Monotonic
outputSyncOrdering SyncAcquire   = Acquire
outputSyncOrdering SyncRelease   = Release
outputSyncOrdering SyncAcqRel    = AcquireRelease
outputSyncOrdering SyncSeqCst    = SequentiallyConsistent

-- The st (single-thread) boolean might need to be negated.
outputFence :: Bool -> LlvmSyncOrdering -> [MetaAnnot] -> Named Instruction --done
outputFence st ord metas = Do $ Fence atom metas'
    where atom = Atomicity st (outputSyncOrdering ord)
          metas' = outputMetaAnnots metas

-- XXX: On x86, vector types need to be 16-byte aligned for aligned access, but
-- we have no way of guaranteeing that this is true with GHC (we would need to
-- modify the layout of the stack and closures, change the storage manager,
-- etc.). So, we blindly tell LLVM that *any* vector store or load could be
-- unaligned. In the future we may be able to guarantee that certain vector
-- access patterns are aligned, in which case we will need a more granular way
-- of specifying alignment.

outputLoad :: LlvmVar -> [MetaAnnot] -> Named Instruction --done
outputLoad var metas
    -- We say the load is volatile and non-atomic.
    | isVecPtrVar var = Do $ Load True op Nothing 1 metas'
    | otherwise       = Do $ Load True op Nothing 0 metas'
  where
    isVecPtrVar = isVector . pLower . getVarType
    op = llvmVarToOperand var
    metas' = outputMetaAnnots metas

outputStore :: LlvmVar -> LlvmVar -> [MetaAnnot] -> Named Instruction --done
outputStore val dst metas
    -- We say the store is volatile and non-atomic.
    | isVecPtrVar dst = Do $ Store True dstOp valOp Nothing 1 metas'
    | otherwise       = Do $ Store True dstOp valOp Nothing 0 metas'
  where
    isVecPtrVar = isVector . pLower . getVarType
    dstOp = llvmVarToOperand dst
    valOp = llvmVarToOperand val
    metas' = outputMetaAnnots metas

outputCast :: LlvmCastOp -> LlvmVar -> LlvmType -> [MetaAnnot] -> Named Instruction --done
outputCast op var ty metas = Do $
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
             $ (llvmVarToOperand op) (llvmTypeToType typeTo) (outputMetaAnnots metas)

-- As of LLVM 3.0, malloc is no longer an instruction of the LLVM IR.
-- One solution to deal with this is to call the @malloc function instead. It may also be possible
-- to replace it with alloca instruction(s), or just not generate mallocs in the first place.
-- I think we can get away without generating these in the first place.
outputMalloc :: LlvmType -> Int -> Named Instruction --'done'
outputMalloc tp amount = undefined

-- Must specify a width for the amount of memory requested, assume a 64 bit quantity.
outputAlloca :: LlvmType -> Int -> [MetaAnnot] -> Named Instruction --done
outputAlloca ty amount metas = Do $ Alloca ty' con 0 metas'
    where ty' = llvmTypeToType ty
          con = ConstantOperand (Int 64 amount)
          metas' = outputMetaAnnots metas

outputGetElementPtr :: Bool -> LlvmVar -> [LlvmVar] -> [MetaAnnot] -> Named Instruction --done
outputGetElementPtr inb ptr idx metas = Do $ GetElementPtr inb ptr' idx' metas'
    where ptr' = llvmVarToOperand ptr
          idx' = llvmVarToOperand idx
          metas' = outputMetaAnnots metas

outputReturn :: Maybe LlvmVar -> [MetaAnnot] -> Named Terminator --done
outputReturn var metas = Do $ Ret var' metas'
    where var' = llvmVarToValue =<< var
          metas' = outputMetaAnnots metas

-- Unconditional branch to target
outputBranch :: LlvmVar -> [MetaAnnot] -> Named Terminator --done
outputBranch var metas = Do $ Br name metas'
    where name = llvmVarToName var
          metas' = outputMetaAnnots metas

outputBranchIf :: LlvmVar -> LlvmVar -> LlvmVar -> [MetaAnnot] -> Named Terminator --done
outputBranchIf cond trueT falseT metas = Do $ CondBr cond' trueT' falseT' metas'
    where cond' = llvmVarToOperand cond
          trueT' = llvmVarToName trueT
          falseT' = llvmVarToName falseT
          metas' = ouputMetaAnnots metas

outputPhi :: LlvmType -> [(LlvmVar,LlvmVar)] -> [MetaAnnot] -> Named Instruction --done
outputPhi ty preds metas = Do $ Phi ty' preds' metas'
    where ty' = llvmTypeToType ty
          preds' = map (\(op,name) -> (llvmVarToOperand op, llvmVarToName name)) preds
          metas' = outputMetaAnnots metas

outputSwitch :: LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> [MetaAnnot] -> Named Terminator --done
outputSwitch op dflt targets metas = Do $ Switch op' dflt' targets' metas'
    where op' = llvmVarToOperand op
          dflt' = llvmVarToName dflt
          targets' = map (\(con, name) -> (llvmVarToConstant con, llvmVarToName name)) targets
          metas' = outputMetaAnnots metas

outputAsm :: LMString -> LMString -> LlvmType -> [LlvmVar] -> Bool -> Bool -> InlineAssembly --done
outputAsm asm constraints rty vars sideeffect alignstack =
    InlineAssembly {
     type' = llvmTypeToType rty,
     assembly = unpackFS asm,
     constraints = unpackFS constraints,
     hasSideEffects = sideeffect,
     alignStack= alignstack,
     dialect = ATTDialect -- Not sure about this, could just as well be intel
    }

-- Get a value from a vector
outputExtract :: LlvmVar -> LlvmVar -> [MetaAnnot] -> Named Instruction --done
outputExtract vec idx metas = Do $ ExtractElement vec' idx' metas'
    where vec' = llvmVarToOperand vec
          idx' = llvmVarToOperand idx
          metas' = outputMetaAnnots metas

-- Insert a value into a vector
outputInsert :: LlvmVar -> LlvmVar -> LlvmVar -> [MetaAnnot] -> Named Instruction --done
outputInsert vec elt idx metas = Do $ InsertElement vec' elt' idx' metas'
    where vec' = llvmVarToOperand vec
          elt' = llvmVarToOperand elt
          idx' = llvmVarToOperand idx
          metas' = outputMetaAnnots metas

outputMetaAnnots :: [MetaAnnot] -> [InstructionMetadata] --done
outputMetaAnnots metas = map outputMetaAnnot metas

outputMetaAnnots :: MetaAnnot -> InstructionMetadata --done
outputMetaAnnots (MetaAnnot str expr) = (unpackFS str, metaExprToMetadataNode expr)