#include <stdlib.h>
#include <stdio.h>
#include "securesoho_wireless.h"
#include "util_wireless.h"
//#include "../osd/cp/DMA_Controller.h"

struct Wireless_Event
{
	void (*PreSelect)(void* object,fd_set *readset, fd_set *writeset, fd_set *errorset, int* blocktime);
	void (*PostSelect)(void* object,int slct, fd_set *readset, fd_set *writeset, fd_set *errorset);
	void (*Destroy)(void* object);

	struct rtnl_handle *rth;
};

struct Wireless_Event *wevent_object = NULL;

void WEVENT_Preselect(void* object,fd_set *readset, fd_set *writeset, fd_set *errorset, int* blocktime)
{
	struct rtnl_handle *rth = (struct rtnl_handle *)((struct Wireless_Event *)object)->rth;

	FD_SET(rth->fd, readset);
}

void WEVENT_Postselect(void* object,int slct, fd_set *readset, fd_set *writeset, fd_set *errorset)
{
	struct rtnl_handle *rth = (struct rtnl_handle *)((struct Wireless_Event *)object)->rth;

	if(FD_ISSET(rth->fd, readset))
		handle_netlink_events(rth);
}

void WEVENT_Destroy(void* object)
{
}

