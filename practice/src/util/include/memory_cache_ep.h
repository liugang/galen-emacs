#ifndef __MEMORY_CACHE_EP_H__
#define __MEMORY_CACHE_EP_H__

typedef enum _enum_memory_cache_event{
	memory_cache_event_malloc,
	memory_cache_event_free,
}_enum_memory_cache_event;

#define MEMORY_CACHE_P2SFIFO	"memory_cache_p2sfifo"
#define MEMORY_CACHE_S2PFIFO	"memory_cache_s2pfifo"

typedef enum _enum_memory_cache_status{
	memory_cache_unknown,
	malloc_HD_memory_cache_success,
	malloc_SD_memory_cache_success,
	malloc_memory_cache_fail,
	malloc_memory_cache_unknown,
	free_memory_cache_success,
	free_memory_cache_unknown,
}enum_memory_cache_status;

enum_memory_cache_status memory_cache_malloc(void);
enum_memory_cache_status memory_cache_free(void);
int memory_cache_event_proxy_init();
#endif /* __SAMBA_EP_H__*/
