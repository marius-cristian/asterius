{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Passes.SafeCCall
  ( splitFunction,
  )
where

import Asterius.EDSL
import Asterius.Internals
import Asterius.Types
import Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Language.Haskell.GHC.Toolkit.Constants

data SafeCCall
  = SafeCCall
      AsteriusEntitySymbol
      Expression

data BlockChunk
  = BlockChunk
      [Expression]
      SafeCCall

data BlockChunks
  = BlockChunks
      [BlockChunk]
      [Expression]

data BSState
  = BSState
      { bsId :: Int,
        bsMap :: M.Map SBS.ShortByteString RelooperBlock,
        bsHookMap :: IM.IntMap SBS.ShortByteString,
        bsPreBlockInstrs :: [Expression]
      }

splitFunction :: AsteriusEntitySymbol -> Function -> Function
splitFunction sym Function {..} =
  Function
    { functionType = functionType,
      varTypes = varTypes,
      body = case body of
        CFG g -> CFG $ splitAllBlocks sym varTypes g
        _ -> body
    }

splitAllBlocks ::
  AsteriusEntitySymbol -> [ValueType] -> RelooperRun -> RelooperRun
splitAllBlocks sym vts r@RelooperRun {..}
  | has_safe_ccall =
    r
      { entry = entry_hook_k,
        blockMap = M.insert entry_hook_k entry_hook_block $ bsMap final_state
      }
  | otherwise = r
  where
    init_state = initBSState entry
    final_state =
      execState
        ( M.foldlWithKey'
            (\m k b -> splitSingleBlock vts save_instrs k b *> m)
            (pure ())
            blockMap
        )
        init_state
    has_safe_ccall = M.size (bsMap final_state) /= M.size blockMap
    (entry_hook_k, entry_hook_block) = entryHookBlock final_state load_instrs
    (save_instrs, load_instrs) = genSaveLoad sym vts

initBSState :: SBS.ShortByteString -> BSState
initBSState entry_k =
  BSState
    { bsId = 1,
      bsMap = M.empty,
      bsHookMap = IM.singleton 0 entry_k,
      bsPreBlockInstrs = []
    }

entryHookBlock ::
  BSState -> [Expression] -> (SBS.ShortByteString, RelooperBlock)
entryHookBlock BSState {..} load_instrs =
  ( "ci",
    RelooperBlock
      { addBlock =
          AddBlockWithSwitch
            { code = concatExpressions load_instrs,
              condition = GetLocal {index = 2, valueType = I32}
            },
        addBranches =
          [ AddBranchForSwitch {to = k, indexes = [fromIntegral i]}
            | (i, k) <- IM.toList bsHookMap
          ]
            <> [ AddBranch
                   { to = "__asterius_unreachable",
                     addBranchCondition = Nothing
                   }
               ]
      }
  )

splitSingleBlock ::
  [ValueType] ->
  [Expression] ->
  SBS.ShortByteString ->
  RelooperBlock ->
  State BSState ()
splitSingleBlock vts save_instrs base_k base_block@RelooperBlock {..} =
  case splitBlockBody $ code addBlock of
    BlockChunks [] _ -> insert_block base_k base_block
    BlockChunks chunks rest_instrs -> do
      for_ (zip chunks (True : repeat False)) produce_block
      modify' w
      where
        w s =
          s
            { bsId = next_id,
              bsMap = M.insert this_k this_block $ bsMap s,
              bsPreBlockInstrs = []
            }
          where
            this_id = bsId s
            next_id = succ this_id
            this_k = "c" <> showSBS this_id
            this_preblock_instrs = bsPreBlockInstrs s
            this_block =
              RelooperBlock
                { addBlock =
                    addBlock
                      { code =
                          concatExpressions $
                            this_preblock_instrs
                              <> rest_instrs
                      },
                  addBranches = addBranches
                }
  where
    insert_block :: SBS.ShortByteString -> RelooperBlock -> State BSState ()
    insert_block new_k new_block =
      modify' $ \s -> s {bsMap = M.insert new_k new_block $ bsMap s}
    produce_block :: (BlockChunk, Bool) -> State BSState ()
    produce_block (BlockChunk block_instrs (SafeCCall _ orig_instr), is_head) =
      modify' w
      where
        w s =
          s
            { bsId = next_id,
              bsMap = M.insert this_k this_block $ bsMap s,
              bsHookMap = IM.insert next_id next_k $ bsHookMap s,
              bsPreBlockInstrs = next_preblock_instrs
            }
          where
            this_id = bsId s
            next_id
              | is_head = this_id
              | otherwise = succ this_id
            this_k
              | is_head = base_k
              | otherwise = "c" <> showSBS this_id
            next_k = "c" <> showSBS next_id
            this_preblock_instrs = bsPreBlockInstrs s
            save_target_instr =
              SetLocal {index = 2, value = ConstI32 $ fromIntegral next_id}
            reset_instrs =
              -- Reset:
              --  * saved local regs
              --  * ffi_func
              --  * ffi_return
              --
              [ Store
                  { bytes = 8,
                    offset = fromIntegral (error "offset_StgTSO_ffi_func"),
                    ptr = wrapInt64 $ getLVal currentTSO,
                    value = ConstI64 0,
                    valueType = I64
                  }
              ]
                ++ [ Store
                       { bytes = 8,
                         offset = fromIntegral (error "offset_StgTSO_ffi_return"),
                         ptr = wrapInt64 $ getLVal currentTSO,
                         value = ConstI64 0,
                         valueType = I64
                       }
                   ]
                ++ [ Store
                       { bytes = 8,
                         offset = i * 8 + fromIntegral (error "offset_StgTSO_saved_regs"),
                         ptr = wrapInt64 $ getLVal currentTSO,
                         value = ConstI64 0,
                         valueType = I64
                       }
                     | i <- [0 .. fromIntegral maxLocalRegs -1] -- TODO: use a loop
                   ]
            (ccall_instr, next_preblock_instrs) = case orig_instr of
              Call {} ->
                ( orig_instr
                    { operands = currentTID : operands orig_instr,
                      callReturnTypes = []
                    },
                  reset_instrs
                )
              SetLocal {value = c@Call {}, ..} ->
                ( c
                    { operands = currentTID : operands c,
                      callReturnTypes = []
                    },
                  [ orig_instr
                      { value =
                          Load
                            { signed = False,
                              bytes = case vts !! fromIntegral index of
                                I32 -> 4
                                F32 -> 4
                                _ -> 8,
                              offset = fromIntegral (error "offset_StgTSO_ffi_return"),
                              valueType = vts !! fromIntegral index,
                              ptr = wrapInt64 $ getLVal currentTSO
                            }
                      }
                  ]
                    ++ reset_instrs
                )
              _ ->
                error
                  "Asterius.Passes.SafeCCall.splitSingleBlock: invalid ccall instruction"
            this_block =
              RelooperBlock
                { addBlock =
                    AddBlock
                      { code =
                          concatExpressions $
                            this_preblock_instrs
                              <> block_instrs
                              <> ( ccall_instr
                                     : save_target_instr
                                     : save_instrs
                                 )
                      },
                  addBranches = []
                }

splitBlockBody :: Expression -> BlockChunks
splitBlockBody instrs = foldr' w (BlockChunks [] []) es
  where
    get_safe t =
      case AsteriusEntitySymbol . SBS.toShort <$> BS.stripSuffix "_wrapper" bs_t of
        Just k | "__asterius_jsffi_async_" `BS.isPrefixOf` bs_t -> Just k
        _ -> Nothing
      where
        bs_t = SBS.fromShort (entityName t)
    add_regular instr (BlockChunks (BlockChunk chunk_instrs ccall : cs) tail_instrs) =
      BlockChunks (BlockChunk (instr : chunk_instrs) ccall : cs) tail_instrs
    add_regular instr (BlockChunks [] tail_instrs) =
      BlockChunks [] (instr : tail_instrs)
    add_ccall k instr (BlockChunks cs tail_instrs) =
      BlockChunks (BlockChunk [] (SafeCCall k instr) : cs) tail_instrs
    w instr@Call {..} cs = case get_safe target of
      Just k -> add_ccall k instr cs
      _ -> add_regular instr cs
    w orig_instr@SetLocal {value = Call {..}} cs = case get_safe target of
      Just k -> add_ccall k orig_instr cs
      _ -> add_regular orig_instr cs
    w instr cs = add_regular instr cs
    es = case instrs of
      Block {..}
        | SBS.null name && null blockReturnTypes -> bodys
        | otherwise ->
          error
            "Asterius.Passes.SafeCCall.splitBlockBody: invalid block"
      Nop -> []
      _ -> [instrs]

-- | Maximal number of local registers
--
-- IMPORTANT: this number must match StgTSO.saved_regs fields (in
-- ghc/include/rts/storage/TSO.h)
maxLocalRegs :: Word
maxLocalRegs = 128

genSaveLoad ::
  AsteriusEntitySymbol -> [ValueType] -> ([Expression], [Expression])
genSaveLoad sym vts
  | fromIntegral (length vts) > maxLocalRegs =
    error
      "Asterius.Passes.SafeCCall.genSaveLoad: too many local regs!"
  | otherwise = (save_instrs, load_instrs)
  where
    -- Don't save local registers 0 and 1 (reserved by the relooper and the
    -- block splitter)
    I32 : I32 : ctx_regs@(I32 : _) = vts
    p_ctx_regs = zip [2 ..] ctx_regs
    pos i = (i - 2) * 8
    bs vt = case vt of
      I32 -> 4
      F32 -> 4
      _ -> 8
    save_instrs =
      [ Store
          { bytes = bs vt,
            offset = pos i + fromIntegral (error "offset_StgTSO_saved_regs"),
            ptr = wrapInt64 $ getLVal currentTSO,
            value = GetLocal {index = i, valueType = vt},
            valueType = vt
          }
        | (i, vt) <- p_ctx_regs
      ]
        <> [ Store
               { bytes = 8,
                 offset = fromIntegral $ (error "offset_StgTSO_ffi_func"),
                 ptr = wrapInt64 $ getLVal currentTSO,
                 value = Symbol {unresolvedSymbol = sym, symbolOffset = 0},
                 valueType = I64
               },
             Store
               { bytes = 8,
                 offset =
                   fromIntegral $
                     offset_Capability_r
                       + offset_StgRegTable_rRet,
                 ptr =
                   wrapInt64
                     Symbol
                       { unresolvedSymbol = "MainCapability",
                         symbolOffset = 0
                       },
                 value = ConstI64 $ fromIntegral ret_ThreadBlocked,
                 valueType = I64
               },
             Store
               { bytes = 2,
                 offset =
                   fromIntegral $
                     offset_StgTSO_why_blocked,
                 ptr = wrapInt64 $ getLVal currentTSO,
                 value = ConstI32 $ fromIntegral blocked_BlockedOnCCall,
                 valueType = I32
               },
             ReturnCall {returnCallTarget64 = "StgReturn"}
           ]
    load_instrs =
      [ SetLocal
          { index = i,
            value =
              Load
                { signed = False,
                  bytes = bs vt,
                  offset = pos i + fromIntegral (error "offset_StgTSO_saved_regs"),
                  valueType = vt,
                  ptr = wrapInt64 $ getLVal currentTSO
                }
          }
        | (i, vt) <- p_ctx_regs
      ]

concatExpressions :: [Expression] -> Expression
concatExpressions es = case es of
  [] -> Nop
  [e] -> e
  _ -> Block {name = "", bodys = es, blockReturnTypes = []}
