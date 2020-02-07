import * as rtsConstants from "./rts.constants.mjs";

/**
 * Class implementing the allocation of nurseries,
 * and also individual heap objects.
 * In the asterius RTS - contrary to GHC - we don't
 * really distinguish between "blocks" and "MBlocks"
 * ("megablocks", "em-blocks"); here all blocks are
 * really MBlocks. MBlocks have a fixed size of 1MiB
 * and are allocated by {@link Memory}. Moreover,
 * MBlocks can be chained to form MegaGroups.
 * For more information on (mega)block allocation, see
 * {@link https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/block-alloc}.
 */
export class HeapAlloc {
  constructor(memory) {
    /**
     * @type Memory
     * @name HeapAlloc#memory
     */
    this.memory = memory;
    /**
     * An array with two entries:
     * 1. The unpinned pool, i.e. the address of the
     *    block descriptor for the MBlock where
     *    unpinned objects are allocated,
     * 2. The pinned pool, i.e. the address of the
     *    block descriptor for the MBlock where
     *    pinned objects are allocated.
     * @name HeapAlloc#currentPools
     */
    this.currentPinned = undefined;
    this.currentUnpinned = undefined;
    // AC
    this.generations = new Array(2); // 2 generations
    /**
     * The set of all currently allocated MegaGroups.
     */
    this.mgroups = new Set();
    // Object.freeze(this);
  }

  /**
   * Initializes the pinned & unpinned pools.
   */
  init() {
    this.currentPinned = this.allocMegaGroup(1, true);
    this.setGenerationNo(0);
  }
  /**
   * Initializes only the unpinned pool.
   * FIXME:
   */
  setGenerationNo(gen_no) {
    this.currentUnpinned = this.generations[gen_no];
    if (!this.currentUnpinned) {
      this.currentUnpinned = this.allocMegaGroup(1, false, gen_no);
      this.generations[gen_no] = this.currentUnpinned;
    }
  }

  /**
   * Allocates a new MegaGroup of enough MBlocks to
   * accommodate the supplied amount of bytes.
   * @param b The number of bytes to allocate
   * @param pinned
   * @param gen_no
   * @returns The address of the block descriptor
   *  of the first MBlock of the MegaGroup.
   */
  hpAlloc(b, pinned, gen_no) {
    const mblocks =
        b <= rtsConstants.sizeof_first_mblock
          ? 1
          : 1 +
            Math.ceil(
              (b - rtsConstants.sizeof_first_mblock) / rtsConstants.mblock_size
            ),
      bd = this.allocMegaGroup(mblocks, pinned, gen_no);
    return bd;
  }

  /**
   * Allocates enough blocks to accommodate the given number
   * of words in the appropriate pool.
   * @param n The number of (64 bit) words to allocate
   * @param pinned Whether to allocate in the pinned pool
   */
  allocate(n, pinned=false) {
    let b = n << 3; // The size in bytes
    // Large objects are forced to be pinned as well
    // (by large, we mean >= 4KiB):
    pinned = pinned || b >= rtsConstants.block_size;
    let pool = pinned ? this.currentPinned : this.currentUnpinned,
      start = Number(
        this.memory.i64Load(pool + rtsConstants.offset_bdescr_start)
      ),
      free = Number(
        this.memory.i64Load(pool + rtsConstants.offset_bdescr_free)
      );
    const blocks = this.memory.i32Load(
        pool + rtsConstants.offset_bdescr_blocks
      ),
      limit = start + rtsConstants.block_size * blocks,
      new_free = free + b;

    if (new_free <= limit) {
      // if the pool has enough space
      this.memory.i64Store(
        pool + rtsConstants.offset_bdescr_free,
        new_free
      );
    } else {
      // not enough space in the corresponding pool,
      // allocate a new one
      // AC FIXME
      if (pinned) {
        pool = this.hpAlloc(b, true);
        this.currentPinned = pool;
      } else {
        const gen_no = this.memory.i16Load(pool - rtsConstants.offset_first_bdescr + rtsConstants.offset_first_block);
        console.log(gen_no);
        pool = this.hpAlloc(b, false, gen_no);
        this.currentUnpinned = pool;
        this.generations[gen_no] = pool;
      }
      free = pool - rtsConstants.offset_first_bdescr + rtsConstants.offset_first_block;
      this.memory.i64Store(
        pool + rtsConstants.offset_bdescr_free,
        free + b
      );
    }
    return free;
  }

  /**
   * Allocates the given number of words in the pinned pool.
   * @param n The number of (64 bit) words to allocate
   */
  allocatePinned(n) {
    return this.allocate(n, true);
  }

  /**
   * Allocates a new MegaGroup of size the supplied number of MBlocks.
   * @param n The number of requested MBlocks
   * @param pinned FIXME:
   * @param gen_no FIXME:
   * @return The address of the block descriptor
   *  of the first MBlock of the MegaGroup
   */
  allocMegaGroup(n, pinned=false, gen_no=0) {
    const req_blocks =
        (rtsConstants.mblock_size * n - rtsConstants.offset_first_block) /
        rtsConstants.block_size,
      mblock = this.memory.getMBlocks(n),
      bd = mblock + rtsConstants.offset_first_bdescr,
      block_addr = mblock + rtsConstants.offset_first_block;
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_start, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_free, block_addr);
    this.memory.i64Store(bd + rtsConstants.offset_bdescr_link, 0);
    this.memory.i16Store(bd + rtsConstants.offset_bdescr_node, n);
    this.memory.i32Store(bd + rtsConstants.offset_bdescr_blocks, req_blocks);
    if (pinned) {
      this.memory.i16Store(bd + rtsConstants.offset_bdescr_flags, rtsConstants.BF_PINNED);
    } else {
      this.memory.i16Store(bd + rtsConstants.offset_bdescr_gen_no, gen_no);
    }
    this.mgroups.add(bd);
    return bd;
  }

  /**
   * Frees the garbage MBlocks by taking into account the
   * information on live and dead MBlocks passed by the 
   * garbage collector. Used by {@link GC#performGC}.
   * @param live_mblocks The set of current live MBlocks
   * @param live_mblocks The set of current dead MBlocks
   * @param minor TODO:
   */
  handleLiveness(live_mblocks, dead_mblocks, major=false) {
    for (const bd of live_mblocks) {
      if (!this.mgroups.has(bd)) {
        throw new WebAssembly.RuntimeError(
          `Invalid live mblock 0x${bd.toString(16)}`
        );
      }
    }
    // Free MBlocks that have been copied during GC
    for (const bd of dead_mblocks) {
      if (!this.mgroups.has(bd)) {
        throw new WebAssembly.RuntimeError(
          `Invalid dead mblock 0x${bd.toString(16)}`
        );
      }
      this.mgroups.delete(bd);
      const p = bd - rtsConstants.offset_first_bdescr,
        n = this.memory.i16Load(bd + rtsConstants.offset_bdescr_node);
      this.memory.freeMBlocks(p, n);
    }
    // Free unreachable MBlocks
    if (major){
      // FIXME: avoid to mistakenly free pinned mblocks
      for (const bd of Array.from(this.mgroups)) {
        if (!live_mblocks.has(bd)) {
          this.mgroups.delete(bd);
          const p = bd - rtsConstants.offset_first_bdescr,
            n = this.memory.i16Load(bd + rtsConstants.offset_bdescr_node);
          this.memory.freeMBlocks(p, n);
        }
      }
      if (!this.mgroups.has(this.currentPinned)) { // pinned mblock
        this.currentPinned = this.allocMegaGroup(1, true);
      }
    }
    // Reinitialize generations & pools if necessary
    // console.log(this.currentUnpinned, this.generations[0], this.generations[1]);
    // console.log(this.mgroups, this.mgroups.has(this.generations[0]));
    // console.log(this.currentPinned);
    for (let i=0; i < this.generations.length; i++)
      if (!this.mgroups.has(this.generations[i])) {
        this.generations[i] = undefined;
      }
    // AC: set gen_no back to 0
    this.setGenerationNo(0);
  }

  /**
   * Estimates the size of living objects by counting the number
   * of MBlocks that were allocated by {@link GC#getMBlocks} 
   * some time ago, but have not been yet been freed by {@link GC#freeMBlocks}.
   * @returns The number of allocated MBlocks
   */
  liveSize() {
    let acc = 0;
    for (const bd of this.mgroups) {
      acc += this.memory.i16Load(bd + rtsConstants.offset_bdescr_node);
    }
    return acc;
  }
}
