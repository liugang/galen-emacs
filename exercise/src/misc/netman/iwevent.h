#include <linux/wireless.h>

struct rtnl_handle
{
	int			fd;
	struct sockaddr_nl      local;
	struct sockaddr_nl      peer;
	__u32                   seq;
	__u32                   dump;
};

void rtnl_close(struct rtnl_handle *rth);
int rtnl_open(struct rtnl_handle *rth, unsigned subscriptions);
void handle_netlink_events(struct rtnl_handle *	rth);
