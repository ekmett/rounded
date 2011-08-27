/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2008
 *
 * ---------------------------------------------------------------------------*/

/* TODO: do we need PosixSource.h ? it lives in rts/ not public includes/ */
/* #include "PosixSource.h" */
#include "Rts.h"

#include "gmp.h"
#include <dlfcn.h>
#include <string.h>
#include <sys/mman.h>
#include <limits.h>
#ifndef PAGESIZE
#define PAGESIZE 4096
#endif

static void * stgAllocForMPFR   (size_t size_in_bytes);
static void * stgReallocForMPFR (void *ptr, size_t old_size, size_t new_size);
static void   stgDeallocForMPFR (void *ptr STG_UNUSED, size_t size STG_UNUSED);

static void initAllocForMPFR( void ) __attribute__((constructor));

extern void *__gmp_allocate_func;
extern void *__gmp_reallocate_func;
extern void *__gmp_free_func;


static void *cache_fn_start;
static void *cache_fn_end;

#define IN_CACHE_FN(addr) ((addr) >= cache_fn_start && (addr) < cache_fn_end)

extern void mpfr_cache();

//////////////////////////////

void __attribute__((used)) noopForMPFR(void *alloc_fn, void *realloc_fn, void *free_fn) {
}

static void find_cache_fn() {
  cache_fn_start = mpfr_cache;

  Dl_info info;
  dladdr(cache_fn_start, &info);

  // This is linear in the length of the cache function.
  // It could be done more efficiently with a binary search that
  // probes outwards exponentially before refining.
  cache_fn_end = cache_fn_start;
  do {
    cache_fn_end = (long *)cache_fn_end + 1;
    dladdr(cache_fn_end, &info);
  } while (!strcmp(info.dli_sname, "mpfr_cache"));

  cache_fn_end = (long *)cache_fn_end - 1;
}

void __attribute__((used)) setAllocForMPFR( void )
{
  if (!cache_fn_start) {
    find_cache_fn();
  }
  mp_set_memory_functions(stgAllocForMPFR, stgReallocForMPFR, stgDeallocForMPFR);
}

static void initAllocForMPFR( void )
{  
  void *mem_ptr = (void *)((long)mp_set_memory_functions & ~(PAGESIZE - 1));
  
  setAllocForMPFR();
  
  if (mprotect(mem_ptr, PAGESIZE, PROT_WRITE)) {
    perror("Couldn't mprotect");
    exit(-1);
  }

  memcpy(mp_set_memory_functions, noopForMPFR, 8);

  if (mprotect(mem_ptr, PAGESIZE, PROT_READ | PROT_EXEC)) {
    perror("Couldn't mprotect");
    exit(-1);
  }
}


/* -----------------------------------------------------------------------------
   Allocation functions for GMP.

   These all use the allocate() interface - we can't have any garbage
   collection going on during a gmp operation, so we use allocate()
   which always succeeds.  The gmp operations which might need to
   allocate will ask the storage manager (via doYouWantToGC()) whether
   a garbage collection is required, in case we get into a loop doing
   only allocate() style allocation.
   -------------------------------------------------------------------------- */

static void *
stgAllocForMPFR (size_t size_in_bytes)
{
  void *ret0 = __builtin_return_address(0);
  void *ret1 = __builtin_return_address(1);
  void *ret2 = __builtin_return_address(2);

  if (IN_CACHE_FN(ret2) || IN_CACHE_FN(ret1) || IN_CACHE_FN(ret0)) {
    //printf("Calling malloc!\n");
    return malloc(size_in_bytes);
  } else {
    //printf("Calling GHC allocator\n");
    StgArrWords* arr;
    nat data_size_in_words, total_size_in_words;

    //fprintf(stderr, "Allocating %zu bytes\n", size_in_bytes);

    /* round up to a whole number of words */
    data_size_in_words  = ROUNDUP_BYTES_TO_WDS(size_in_bytes);
    total_size_in_words = sizeofW(StgArrWords) + data_size_in_words;

    /* allocate and fill it in. */
    arr = (StgArrWords *)allocate(rts_unsafeGetMyCapability(), total_size_in_words);
    SET_ARR_HDR(arr, &stg_ARR_WORDS_info, CCCS, size_in_bytes);

    /* and return a ptr to the goods inside the array */
    return arr->payload;
  }
}

static void *
stgReallocForMPFR (void *ptr, size_t old_size, size_t new_size)
{
  size_t min_size;
  
  void *new_stuff_ptr = stgAllocForMPFR(new_size);
  nat i = 0;
  char *p = (char *) ptr;
  char *q = (char *) new_stuff_ptr;

  min_size = old_size < new_size ? old_size : new_size;
  /* TODO: use memcpy */
  for (; i < min_size; i++, p++, q++) {
      *q = *p;
  }

  return(new_stuff_ptr);
}

static void
stgDeallocForMPFR (void *ptr, size_t size STG_UNUSED)
{
  void *ret0 = __builtin_return_address(0);
  void *ret1 = __builtin_return_address(1);
  void *ret2 = __builtin_return_address(2);

  if (IN_CACHE_FN(ret2) || IN_CACHE_FN(ret1) || IN_CACHE_FN(ret0)) {
    //printf("Calling free!\n");
    free(ptr);
  }
  /* easy for us: the garbage collector does the dealloc'n */
}