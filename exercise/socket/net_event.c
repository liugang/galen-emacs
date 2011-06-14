#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netinet/ether.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>


struct nm_rtnl_handle {
	int sk;
};

static int nm_rtnl_handle_init(struct nm_rtnl_handle *rtnl);
static int nm_rtnl_handle_fini(struct nm_rtnl_handle *rtnl);
static int nm_rtnl_handle_get_fd(struct nm_rtnl_handle *rtnl);
static int nm_rtnl_handle_events(struct nm_rtnl_handle *rtnl);

static void rtnl_newlink(struct nlmsghdr *hdr);
static void rtnl_dellink(struct nlmsghdr *hdr);
static void rtnl_newaddr(struct nlmsghdr *hdr);
static void rtnl_deladdr(struct nlmsghdr *hdr);
static void rtnl_newroute(struct nlmsghdr *hdr);
static void rtnl_delroute(struct nlmsghdr *hdr);

static void rtnl_message(void *buf, size_t len);

static int nm_rtnl_handle_init(struct nm_rtnl_handle *rth)
{
	int sk;
	struct sockaddr_nl addr;

//	mylog_debug("enter %s", __func__);

	if (!rth){
//		mylog_error("Failed to get the rtnl handle");
		return -1;
	}

	/* http://www.digipedia.pl/man/doc/view/PF_NETLINK.7/ */
	// #include <asm/types.h>
	// #include <sys/socket.h>
	// #include <linux/netlink.h> 
	//  PF_NETLINK defined in /usr/include/bits/socket.h, which included by /usr/include/sys/socket.h
	// netlink - Communication between kernel and userspace (PF_NETLINK)
	// #define NETLINK_ROUTE 0 // Routing/device hook  linux/netlink.h
	sk = socket(PF_NETLINK, SOCK_DGRAM, NETLINK_ROUTE);
	if (sk < 0)
		return -1;

	memset(&addr, 0, sizeof(addr));
	addr.nl_family = AF_NETLINK;
	addr.nl_groups = RTMGRP_LINK | RTMGRP_IPV4_IFADDR | RTMGRP_IPV4_ROUTE;

	if (bind(sk, (struct sockaddr *) &addr, sizeof(addr)) < 0) {
		close(sk);
		return -1;
	}

	rth->sk = sk;
	return 0;
}

static int nm_rtnl_handle_fini(struct nm_rtnl_handle *rth)
{
	if (!rth){
//		mylog_error("The rth handle is null");
		return -1;
	}

	if (rth->sk < 0){
//		mylog_error("the socket is null");
		return -1;
	}

	close(rth->sk);
	rth->sk = -1;
	return 0;
}

static int nm_rtnl_handle_get_fd(struct nm_rtnl_handle *rth)
{
	if (!rth){
//		mylog_error("The rth handle is null");
		return -1;
	}

	return rth->sk;
}

static int nm_rtnl_handle_events(struct nm_rtnl_handle *rth)
{
	printf("\033[1;45m ===============  %s(), %d... ================== \033[0m  \n", __FUNCTION__, __LINE__);
	struct sockaddr_nl sanl;
	socklen_t sanllen = sizeof(struct sockaddr_nl);

	int len;
	char buf[8192];

	if (!rth){
//		mylog_error("the rtnl handle is null");
		return -1;
	}
	if (rth->sk < 0){
//		mylog_error("the socket is null");
		return -1;
	}

	len = recvfrom(rth->sk, buf, sizeof(buf), MSG_DONTWAIT, (struct sockaddr*)&sanl, &sanllen);
	if(len < 0)
	{
		if(errno != EINTR && errno != EAGAIN)
		{
//			mylog_error("%s: error reading netlink: %s.",
//					__func__, strerror(errno));
		}
		return -1;
	}

	if(len == 0)
	{
//		mylog_error("%s: EOF on netlink??", __func__);
		return -1;
	}
	printf("\033[1;45m ===============  %s(), %d... ================== \033[0m  \n", __FUNCTION__, __LINE__);

	rtnl_message(buf, len);
	return 0;
}

static void rtnl_newlink(struct nlmsghdr *hdr)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	struct ifinfomsg *msg = (struct ifinfomsg *) NLMSG_DATA(hdr);

//	rtnl_link(hdr);

//	process_newlink(msg->ifi_type, msg->ifi_index, msg->ifi_flags,
//			msg->ifi_change, msg, IFA_PAYLOAD(hdr));
}

static void rtnl_dellink(struct nlmsghdr *hdr)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	struct ifinfomsg *msg = (struct ifinfomsg *) NLMSG_DATA(hdr);

//	rtnl_link(hdr);

//	process_dellink(msg->ifi_type, msg->ifi_index, msg->ifi_flags,
//			msg->ifi_change, msg, IFA_PAYLOAD(hdr));
}

static void rtnl_newaddr(struct nlmsghdr *hdr)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	struct ifaddrmsg *msg = (struct ifaddrmsg *) NLMSG_DATA(hdr);

//	rtnl_addr(hdr);
//
//	process_newaddr(msg->ifa_family, msg->ifa_prefixlen, msg->ifa_index,
//			msg, IFA_PAYLOAD(hdr));
}

static void rtnl_deladdr(struct nlmsghdr *hdr)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	struct ifaddrmsg *msg = (struct ifaddrmsg *) NLMSG_DATA(hdr);

//	rtnl_addr(hdr);
//
//	process_deladdr(msg->ifa_family, msg->ifa_prefixlen, msg->ifa_index,
//			msg, IFA_PAYLOAD(hdr));
}

static void rtnl_newroute(struct nlmsghdr *hdr)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	struct rtmsg *msg = (struct rtmsg *) NLMSG_DATA(hdr);

//	rtnl_route(hdr);
//
//	if (msg->rtm_table == RT_TABLE_MAIN &&
//			msg->rtm_protocol == RTPROT_BOOT &&
//			msg->rtm_type == RTN_UNICAST)
//		process_newroute(msg->rtm_family, msg->rtm_scope,
//				msg, RTM_PAYLOAD(hdr));
}

static void rtnl_delroute(struct nlmsghdr *hdr)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	struct rtmsg *msg = (struct rtmsg *) NLMSG_DATA(hdr);

//	rtnl_route(hdr);
//
//	if (msg->rtm_table == RT_TABLE_MAIN &&
//			msg->rtm_protocol == RTPROT_BOOT &&
//			msg->rtm_type == RTN_UNICAST)
//		process_delroute(msg->rtm_family, msg->rtm_scope,
//				msg, RTM_PAYLOAD(hdr));
}

static void rtnl_message(void *buf, size_t len)
{
//	mylog_debug("buf %p len %zd", buf, len);

	while (len > 0) {
		struct nlmsghdr *hdr = buf;
		struct nlmsgerr *err;

		if (!NLMSG_OK(hdr, len))
			break;

//		mylog_debug("%s len %d type %d flags 0x%04x seq %d",
//				type2string(hdr->nlmsg_type),
//				hdr->nlmsg_len, hdr->nlmsg_type,
//				hdr->nlmsg_flags, hdr->nlmsg_seq);
//		printf("=================%s len %d type %d flags 0x%04x seq %d",
//				type2string(hdr->nlmsg_type),
//				hdr->nlmsg_len, hdr->nlmsg_type,
//				hdr->nlmsg_flags, hdr->nlmsg_seq);

		printf("type = %d\n", hdr->nlmsg_type);
		switch (hdr->nlmsg_type) {
			case NLMSG_NOOP:
			case NLMSG_OVERRUN:
			case NLMSG_DONE:
//				mylog_debug("%s:%d", __func__, __LINE__);
				return;
			case NLMSG_ERROR:
				err = NLMSG_DATA(hdr);
//				mylog_debug("error %d (%s)", -err->error,
//						strerror(-err->error));
				return;
			case RTM_NEWLINK:
				rtnl_newlink(hdr);
				break;
			case RTM_DELLINK:
				rtnl_dellink(hdr);
				break;
			case RTM_NEWADDR:
				rtnl_newaddr(hdr);
				break;
			case RTM_DELADDR:
				rtnl_deladdr(hdr);
				break;
			case RTM_NEWROUTE:
				rtnl_newroute(hdr);
				break;
			case RTM_DELROUTE:
				rtnl_delroute(hdr);
				break;
		}

		len -= hdr->nlmsg_len;
		buf += hdr->nlmsg_len;
	}
}


int main(int argc, char *argv[])
{
	int n;
	fd_set rset;
	struct timeval tv;
	struct nm_rtnl_handle rth;

	/* Open rtnl handle */
	if(nm_rtnl_handle_init(&rth) < 0) {
		perror("Can't initialize rtnetlink handle");
		return(1);
	}

	while (1){
		FD_ZERO(&rset);
		tv.tv_sec = 1; tv.tv_usec = 0;

		FD_SET(nm_rtnl_handle_get_fd(&rth), &rset);

		n = select(FD_SETSIZE, &rset, NULL, NULL, &tv);

		if (n < 0) {
			printf("select:%s\n", strerror(errno));
			goto error;
		}

		if(FD_ISSET(nm_rtnl_handle_get_fd(&rth), &rset)) {
			printf("\033[1;45m ===============  %s(), %d... ================== \033[0m  \n", __FUNCTION__, __LINE__);
			nm_rtnl_handle_events(&rth);
		}
	}
	return 0;

error:
	nm_rtnl_handle_fini(&rth);
//	nm_dbus_srv_close();
	printf("network daemon error\n");
	/* Explicitly call the mylog cleanup routine */
	/* if (mylog_fini()){ */
	/*	printf("mylog_fini() failed"); */
	/* } */

	return -1;
}
