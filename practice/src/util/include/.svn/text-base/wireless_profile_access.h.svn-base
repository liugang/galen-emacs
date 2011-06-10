/* Copyright (C) 2006, Alphanetworks, inc.
 * Author:  Tao Yu<tao_yu@alphanetworks.com>
 * $Header: /data/cvsroot/DMA/util/include/wireless_profile_access.h,v 1.1.2.5 2006-08-30 04:54:17 ytao Exp $
 * vim:cindent:ts=8:
 */
#ifndef	__WIRELESS_PROFILE_ACCESS_H__
#define	__WIRELESS_PROFILE_ACCESS_H__

#include "securesoho_config.h"
#include "kernel_list.h"

typedef struct _wp_node_t{
	SecureSOHO_Wireless w;
	struct list_head list; 
}wp_node;

int	wireless_profile_init(wp_node *wplist, int *count);
int	wireless_profile_destroy(wp_node *wplist);

int	wireless_profile_save(wp_node *wplist);
int	wireless_profile_get_maxcount(void);
int     wireless_profile_getcount(wp_node *wplist);
int 	wireless_profile_contains_of(wp_node *wplist, SecureSOHO_Wireless *w);
int 	wireless_profile_get_nth_node(wp_node *wplist, int pos, wp_node **node);

int	wireless_profile_add(wp_node *wplist, wp_node *node);
int	wireless_profile_add_tail(wp_node *wplist, wp_node *node);
int 	wireless_profile_remove(wp_node *wplist, wp_node *node);
int 	wireless_profile_replace(wp_node *wplist, wp_node *node, int pos);

int	wireless_profile_moveup(wp_node *wplist, wp_node *node);
int	wireless_profile_movedown(wp_node *wplist, wp_node *node);
#endif
