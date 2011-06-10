#ifndef	__MYMALLOC__
#define	__MYMALLOC__

/*
 * none (0):
 *  none -- no functionality (0)
 */
#define DMALLOC_TAG_NONE (0x0)
/*
 * verylow (0x8403)
 *   log-stats -- log general statistics (0x1)
 *   log-non-free -- log non-freed pointers (0x2)
 *   check-fence -- check fence-post errors (0x400)
 *   check-shutdown -- check heap on shutdown (0x8000)
 *
 */
#define DMALLOC_TAG_VERYLOW (0x008102)
/*
 *runtime (0x4000503):
 *   log-stats -- log general statistics (0x1)
 *   log-non-free -- log non-freed pointers (0x2)
 *   log-bad-space -- dump space from bad pnt (0x100)
 *   check-fence -- check fence-post errors (0x400)
 *   catch-null -- abort if no memory available (0x4000000)
 */
#define DMALLOC_TAG_RUN (0x4000503)
/*
 *low (0x4e48503):
 *   log-stats -- log general statistics (0x1)
 *   log-non-free -- log non-freed pointers (0x2)
 *   log-bad-space -- dump space from bad pnt (0x100)
 *   log-elapsed-time -- log elapsed-time for allocated pointer (0x40000)
 *   check-fence -- check fence-post errors (0x400)
 *   check-shutdown -- check heap on shutdown (0x8000)
 *   free-blank -- overwrite freed memory with \0337 byte (0xdf) (0x200000)
 *   error-abort -- abort immediately on error (0x400000)
 *   alloc-blank -- overwrite allocated memory with \0332 byte (0xda) (0x800000)
 *   catch-null -- abort if no memory available (0x4000000)
 */
#define DMALLOC_TAG_LOW (0x4e48503)
/*
 *med (0x4f48d03):
 *   log-stats -- log general statistics (0x1)
 *   log-non-free -- log non-freed pointers (0x2)
 *   log-bad-space -- dump space from bad pnt (0x100)
 *   log-elapsed-time -- log elapsed-time for allocated pointer (0x40000)
 *   check-fence -- check fence-post errors (0x400)
 *   check-heap -- check heap adm structs (0x800)
 *   check-shutdown -- check heap on shutdown (0x8000)
 *   realloc-copy -- copy all re-allocations (0x100000)
 *   free-blank -- overwrite freed memory with \0337 byte (0xdf) (0x200000)
 *   error-abort -- abort immediately on error (0x400000)
 *   alloc-blank -- overwrite allocated memory with \0332 byte (0xda) (0x800000)
 *   catch-null -- abort if no memory available (0x4000000)
 */
#define DMALLOC_TAG_MED (0x4f48d03)
/*
 *high (0x4f4ed03):
 *   log-stats -- log general statistics (0x1)
 *   log-non-free -- log non-freed pointers (0x2)
 *   log-bad-space -- dump space from bad pnt (0x100)
 *   log-elapsed-time -- log elapsed-time for allocated pointer (0x40000)
 *   check-fence -- check fence-post errors (0x400)
 *   check-heap -- check heap adm structs (0x800)
 *   check-blank -- check mem overwritten by alloc-blank, free-blank (0x2000)
 *   check-funcs -- check functions (0x4000)
 *   check-shutdown -- check heap on shutdown (0x8000)
 *   realloc-copy -- copy all re-allocations (0x100000)
 *   free-blank -- overwrite freed memory with \0337 byte (0xdf) (0x200000)
 *   error-abort -- abort immediately on error (0x400000)
 *   alloc-blank -- overwrite allocated memory with \0332 byte (0xda) (0x800000)
 *   catch-null -- abort if no memory available (0x4000000)
 */
#define DMALLOC_TAG_HIGH (0x4f4ed03)
/*
 *all (0xcf4ed2b):
 *   log-stats -- log general statistics (0x1)
 *   log-non-free -- log non-freed pointers (0x2)
 *   log-trans -- log memory transactions (0x8)
 *   log-admin -- log administrative info (0x20)
 *   log-bad-space -- dump space from bad pnt (0x100)
 *   log-elapsed-time -- log elapsed-time for allocated pointer (0x40000)
 *   check-fence -- check fence-post errors (0x400)
 *   check-heap -- check heap adm structs (0x800)
 *   check-blank -- check mem overwritten by alloc-blank, free-blank (0x2000)
 *   check-funcs -- check functions (0x4000)
 *   check-shutdown -- check heap on shutdown (0x8000)
 *   realloc-copy -- copy all re-allocations (0x100000)
 *   free-blank -- overwrite freed memory with \0337 byte (0xdf) (0x200000)
 *   error-abort -- abort immediately on error (0x400000)
 *   alloc-blank -- overwrite allocated memory with \0332 byte (0xda) (0x800000)
 *   catch-null -- abort if no memory available (0x4000000)
 *   never-reuse -- never re-use freed memory (0x8000000)
 */
#define DMALLOC_TAG_ALL (0xcf4ed2b)

#ifdef CONF_DMALLOC
#include "dmalloc.h"

#ifdef CONF_DMALLOC_TAG
#define DMALLOC_TAG_MASK CONF_DMALLOC_TAG
#else
#define DMALLOC_TAG_MASK DMALLOC_TAG_VERYLOW
#endif

#define mymalloc_init(name) \
    do{			\
	char    dmalloc_settings[1024]; \
	sprintf(dmalloc_settings, "debug=0x%x,inter=100,log=%s/dmalloc.%s.%%p", DMALLOC_TAG_MASK, CONF_DMALLOC_LOGDIR, basename(name)); \
	fprintf(stderr, "\033[1;33mDebug with dmalloc\n"); \
	fprintf(stderr, "\t[%s]\033[0m\n", dmalloc_settings); \
	fflush(stderr); \
	dmalloc_debug_setup( dmalloc_settings ); \
    }while(0)
#define mymalloc_log_unfreed dmalloc_log_unfreed
#else
#define mymalloc_init(name) 
#define mymalloc_log_unfreed() 
#endif
#endif	// __MYMALLOC__
