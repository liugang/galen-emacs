#include <sys/types.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <linux/wireless.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#include <sys/time.h>
#include <unistd.h>
#include <time.h>
#include "iwevent.h"
#include "securesoho_network2.h"
#include "securesoho_wireless.h"

/* Ugly backward compatibility :-( */
#ifndef IFLA_WIRELESS
#define IFLA_WIRELESS	(IFLA_MASTER + 1)
#endif /* IFLA_WIRELESS */

/************************ RTNETLINK HELPERS ************************/
/*
 * The following code is extracted from :
 * ----------------------------------------------
 * libnetlink.c	RTnetlink service routines.
 *
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 *
 * Authors:	Alexey Kuznetsov, <kuznet@ms2.inr.ac.ru>
 * -----------------------------------------------
 */

typedef struct stream_descr
{
	char *	end;		/* End of the stream */
	char *	current;	/* Current event in stream of events */
	char *	value;		/* Current value in event */
} stream_descr;

void rtnl_close(struct rtnl_handle *rth)
{
	close(rth->fd);
}

int rtnl_open(struct rtnl_handle *rth, unsigned subscriptions)
{
	int addr_len;

	memset(rth, 0, sizeof(rth));

	rth->fd = socket(PF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
	if (rth->fd < 0) {
		perror("Cannot open netlink socket");
		return -1;
	}

	memset(&rth->local, 0, sizeof(rth->local));
	rth->local.nl_family = AF_NETLINK;
	rth->local.nl_groups = subscriptions;

	if (bind(rth->fd, (struct sockaddr*)&rth->local, sizeof(rth->local)) < 0) {
		perror("Cannot bind netlink socket");
		return -1;
	}
	addr_len = sizeof(rth->local);
	if (getsockname(rth->fd, (struct sockaddr*)&rth->local, &addr_len) < 0) {
		perror("Cannot getsockname");
		return -1;
	}
	if (addr_len != sizeof(rth->local)) {
		DBGP(LS_NETWORK_DAEMON, "Wrong address length %d\n", addr_len);
		return -1;
	}
	if (rth->local.nl_family != AF_NETLINK) {
		DBGP(LS_NETWORK_DAEMON, "Wrong address family %d\n", rth->local.nl_family);
		return -1;
	}
	rth->seq = time(NULL);
	return 0;
}

/************************ EVENT SUBROUTINES ************************/
/*
 * The Wireless Extension API 14 and greater define Wireless Events,
 * that are used for various events and scanning.
 * Those functions help the decoding of events, so are needed only in
 * this case.
 */

/* Type of headers we know about (basically union iwreq_data) */
#define IW_HEADER_TYPE_NULL	0	/* Not available */
#define IW_HEADER_TYPE_CHAR	2	/* char [IFNAMSIZ] */
#define IW_HEADER_TYPE_UINT	4	/* __u32 */
#define IW_HEADER_TYPE_FREQ	5	/* struct iw_freq */
#define IW_HEADER_TYPE_ADDR	6	/* struct sockaddr */
#define IW_HEADER_TYPE_POINT	8	/* struct iw_point */
#define IW_HEADER_TYPE_PARAM	9	/* struct iw_param */
#define IW_HEADER_TYPE_QUAL	10	/* struct iw_quality */

/* Headers for the various requests */
static const char standard_ioctl_hdr[] = {
	IW_HEADER_TYPE_NULL,	/* SIOCSIWCOMMIT */
	IW_HEADER_TYPE_CHAR,	/* SIOCGIWNAME */
	IW_HEADER_TYPE_PARAM,	/* SIOCSIWNWID */
	IW_HEADER_TYPE_PARAM,	/* SIOCGIWNWID */
	IW_HEADER_TYPE_FREQ,	/* SIOCSIWFREQ */
	IW_HEADER_TYPE_FREQ,	/* SIOCGIWFREQ */
	IW_HEADER_TYPE_UINT,	/* SIOCSIWMODE */
	IW_HEADER_TYPE_UINT,	/* SIOCGIWMODE */
	IW_HEADER_TYPE_PARAM,	/* SIOCSIWSENS */
	IW_HEADER_TYPE_PARAM,	/* SIOCGIWSENS */
	IW_HEADER_TYPE_NULL,	/* SIOCSIWRANGE */
	IW_HEADER_TYPE_POINT,	/* SIOCGIWRANGE */
	IW_HEADER_TYPE_NULL,	/* SIOCSIWPRIV */
	IW_HEADER_TYPE_POINT,	/* SIOCGIWPRIV */
	IW_HEADER_TYPE_NULL,	/* SIOCSIWSTATS */
	IW_HEADER_TYPE_POINT,	/* SIOCGIWSTATS */
	IW_HEADER_TYPE_POINT,	/* SIOCSIWSPY */
	IW_HEADER_TYPE_POINT,	/* SIOCGIWSPY */
	IW_HEADER_TYPE_POINT,	/* SIOCSIWTHRSPY */
	IW_HEADER_TYPE_POINT,	/* SIOCGIWTHRSPY */
	IW_HEADER_TYPE_ADDR,	/* SIOCSIWAP */
	IW_HEADER_TYPE_ADDR,	/* SIOCGIWAP */
	IW_HEADER_TYPE_NULL,	/* -- hole -- */
	IW_HEADER_TYPE_POINT,	/* SIOCGIWAPLIST */
	IW_HEADER_TYPE_PARAM,	/* SIOCSIWSCAN */
	IW_HEADER_TYPE_POINT,	/* SIOCGIWSCAN */
	IW_HEADER_TYPE_POINT,	/* SIOCSIWESSID */
	IW_HEADER_TYPE_POINT,	/* SIOCGIWESSID */
	IW_HEADER_TYPE_POINT,	/* SIOCSIWNICKN */
	IW_HEADER_TYPE_POINT,	/* SIOCGIWNICKN */
	IW_HEADER_TYPE_NULL,	/* -- hole -- */
	IW_HEADER_TYPE_NULL,	/* -- hole -- */
	IW_HEADER_TYPE_PARAM,	/* SIOCSIWRATE */
	IW_HEADER_TYPE_PARAM,	/* SIOCGIWRATE */
	IW_HEADER_TYPE_PARAM,	/* SIOCSIWRTS */
	IW_HEADER_TYPE_PARAM,	/* SIOCGIWRTS */
	IW_HEADER_TYPE_PARAM,	/* SIOCSIWFRAG */
	IW_HEADER_TYPE_PARAM,	/* SIOCGIWFRAG */
	IW_HEADER_TYPE_PARAM,	/* SIOCSIWTXPOW */
	IW_HEADER_TYPE_PARAM,	/* SIOCGIWTXPOW */
	IW_HEADER_TYPE_PARAM,	/* SIOCSIWRETRY */
	IW_HEADER_TYPE_PARAM,	/* SIOCGIWRETRY */
	IW_HEADER_TYPE_POINT,	/* SIOCSIWENCODE */
	IW_HEADER_TYPE_POINT,	/* SIOCGIWENCODE */
	IW_HEADER_TYPE_PARAM,	/* SIOCSIWPOWER */
	IW_HEADER_TYPE_PARAM,	/* SIOCGIWPOWER */
};
static const unsigned int standard_ioctl_num = sizeof(standard_ioctl_hdr);

/*
 * Meta-data about all the additional standard Wireless Extension events
 * we know about.
 */
static const char	standard_event_hdr[] = {
	IW_HEADER_TYPE_ADDR,	/* IWEVTXDROP */
	IW_HEADER_TYPE_QUAL,	/* IWEVQUAL */
	IW_HEADER_TYPE_POINT,	/* IWEVCUSTOM */
	IW_HEADER_TYPE_ADDR,	/* IWEVREGISTERED */
	IW_HEADER_TYPE_ADDR,	/* IWEVEXPIRED */
};
static const unsigned int standard_event_num = sizeof(standard_event_hdr);

/* Size (in bytes) of various events */
static const int event_type_size[] = {
	IW_EV_LCP_LEN,		/* IW_HEADER_TYPE_NULL */
	0,
	IW_EV_CHAR_LEN,		/* IW_HEADER_TYPE_CHAR */
	0,
	IW_EV_UINT_LEN,		/* IW_HEADER_TYPE_UINT */
	IW_EV_FREQ_LEN,		/* IW_HEADER_TYPE_FREQ */
	IW_EV_ADDR_LEN,		/* IW_HEADER_TYPE_ADDR */
	0,
	IW_EV_POINT_LEN,	/* Without variable payload */
	IW_EV_PARAM_LEN,	/* IW_HEADER_TYPE_PARAM */
	IW_EV_QUAL_LEN,		/* IW_HEADER_TYPE_QUAL */
};

/*------------------------------------------------------------------*/
/*
 * Initialise the struct stream_descr so that we can extract
 * individual events from the event stream.
 */
void
iw_init_event_stream(struct stream_descr *	stream,	/* Stream of events */
		     char *			data,
		     int			len)
{
  /* Cleanup */
  memset((char *) stream, '\0', sizeof(struct stream_descr));

  /* Set things up */
  stream->current = data;
  stream->end = data + len;
}

/*------------------------------------------------------------------*/
/*
 * Extract the next event from the event stream.
 */
int
iw_extract_event_stream(struct stream_descr *	stream,	/* Stream of events */
			struct iw_event *	iwe)	/* Extracted event */
{
  int		event_type = 0;
  unsigned int	event_len = 1;		/* Invalid */
  char *	pointer;
  /* Don't "optimise" the following variable, it will crash */
  unsigned	cmd_index;		/* *MUST* be unsigned */

  /* Check for end of stream */
  if((stream->current + IW_EV_LCP_LEN) > stream->end)
    return(0);

#if 0
 	DBGP(LS_NETWORK_DAEMON,"DBG - stream->current = %p, stream->value = %p, stream->end = %p\n",
	 stream->current, stream->value, stream->end);
#endif

  /* Extract the event header (to get the event id).
   * Note : the event may be unaligned, therefore copy... */
  memcpy((char *) iwe, stream->current, IW_EV_LCP_LEN);

#if 0
 	DBGP(LS_NETWORK_DAEMON,"DBG - iwe->cmd = 0x%X, iwe->len = %d\n",
	 iwe->cmd, iwe->len);
#endif

  /* Check invalid events */
  if(iwe->len <= IW_EV_LCP_LEN)
    return(-1);

  /* Get the type and length of that event */
  if(iwe->cmd <= SIOCIWLAST)
    {
      cmd_index = iwe->cmd - SIOCIWFIRST;
      if(cmd_index < standard_ioctl_num)
	event_type = standard_ioctl_hdr[cmd_index];
    }
  else
    {
      cmd_index = iwe->cmd - IWEVFIRST;
      if(cmd_index < standard_event_num)
	event_type = standard_event_hdr[cmd_index];
    }
  /* Unknown events -> event_type=0 => IW_EV_LCP_LEN */
  event_len = event_type_size[event_type];

  /* Check if we know about this event */
  if(event_len <= IW_EV_LCP_LEN)
    {
      /* Skip to next event */
      stream->current += iwe->len;
      return(2);
    }
  event_len -= IW_EV_LCP_LEN;

  /* Set pointer on data */
  if(stream->value != NULL)
    pointer = stream->value;			/* Next value in event */
  else
    pointer = stream->current + IW_EV_LCP_LEN;	/* First value in event */

#if 0
 	DBGP(LS_NETWORK_DAEMON,"DBG - event_type = %d, event_len = %d, pointer = %p\n",
	 event_type, event_len, pointer);
#endif

  /* Copy the rest of the event (at least, fixed part) */
  if((pointer + event_len) > stream->end)
    {
      /* Go to next event */
      stream->current += iwe->len;
      return(-2);
    }
  memcpy((char *) iwe + IW_EV_LCP_LEN, pointer, event_len);

  /* Skip event in the stream */
  pointer += event_len;

  /* Special processing for iw_point events */
  if(event_type == IW_HEADER_TYPE_POINT)
    {
      /* Check the length of the payload */
      if((iwe->len - (event_len + IW_EV_LCP_LEN)) > 0)
	/* Set pointer on variable part (warning : non aligned) */
	iwe->u.data.pointer = pointer;
      else
	/* No data */
	iwe->u.data.pointer = NULL;

      /* Go to next event */
      stream->current += iwe->len;
    }
  else
    {
      /* Is there more value in the event ? */
      if((pointer + event_len) <= (stream->current + iwe->len))
	/* Go to next value */
	stream->value = pointer;
      else
	{
	  /* Go to next event */
	  stream->value = NULL;
	  stream->current += iwe->len;
	}
    }
  return(1);
}
/********************* WIRELESS EVENT DECODING *********************/
/*
 * This is the bit I wrote...
 */

#if WIRELESS_EXT > 13
/*------------------------------------------------------------------*/
/*
 * Print one element from the scanning results
 */
static inline int
print_event_token(struct iw_event *	event,	/* Extracted token */
		  char *		ifname,
		  struct iw_range *	iwrange,	/* Range info */
		  int			has_iwrange)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	/* Now, let's decode the event */
	switch(event->cmd)
	{
	/* ----- set events ----- */
	/* Events that result from a "SET XXX" operation by the user */
	case SIOCSIWNWID:
		break;
	case SIOCSIWFREQ:
		break;
	case SIOCSIWMODE:
		break;
	case SIOCSIWESSID:
		break;
	case SIOCSIWENCODE:
		break;
	/* ----- driver events ----- */
	/* Events generated by the driver when something important happens */
	case SIOCGIWAP:
		DBGP(LS_NETWORK_DAEMON,"(%s:%d) SIOCGIWAP\n", __FUNCTION__, __LINE__);
		break;
	case SIOCGIWSCAN:
		DBGP(LS_NETWORK_DAEMON,"Scan request completed\n");
		break;
	case IWEVTXDROP:
		break;
#if WIRELESS_EXT > 14
	case IWEVCUSTOM:
		DBGP(LS_NETWORK_DAEMON,"(%s:%d) IWEVCUSTOM\n", __FUNCTION__, __LINE__);
		break;
	case IWEVREGISTERED:
		wlan->wlan_linkup();
		break;
	case IWEVEXPIRED:
		wlan->wlan_linkdown();
		break;
#endif /* WIRELESS_EXT > 14 */
#if WIRELESS_EXT > 15
	case SIOCGIWTHRSPY:
		DBGP(LS_NETWORK_DAEMON,"(%s:%d) SIOCGIWTHRSPY\n", __FUNCTION__, __LINE__);
		break;
#else
	/* Avoid "Unused parameter" warning */
	ifname = ifname;
#endif /* WIRELESS_EXT > 15 */
	/* ----- junk ----- */
	/* other junk not currently in use */
	case SIOCGIWRATE:
		DBGP(LS_NETWORK_DAEMON,"(%s:%d) SIOCGIWRATE\n", __FUNCTION__, __LINE__);
		break;
	case SIOCGIWNAME:
		DBGP(LS_NETWORK_DAEMON,"(%s:%d) SIOCGIWNAME\n", __FUNCTION__, __LINE__);
		break;
	case IWEVQUAL:
		DBGP(LS_NETWORK_DAEMON,"(%s:%d) IWEVQUAL\n", __FUNCTION__, __LINE__);
		break;
	default:
		DBGP(LS_NETWORK_DAEMON,"(Unknown Wireless event 0x%04X)\n", event->cmd);
	}	/* switch(event->cmd) */
	return(0);
}

/*------------------------------------------------------------------*/
/*
 * Print out all Wireless Events part of the RTNetlink message
 * Most often, there will be only one event per message, but
 * just make sure we read everything...
 */
static inline int
print_event_stream(char *	ifname,
		   char *	data,
		   int		len)
{
  struct iw_event	iwe;
  struct stream_descr	stream;
  int			ret;
  struct timeval	recv_time;
#if 0
  struct iw_range	range;
  int			has_range;
#endif

#if 0
  has_range = (iw_get_range_info(skfd, ifname, &range) < 0);
#endif

  /* In readable form */
  gettimeofday(&recv_time, NULL);
#if 0 // Steven Kuo: We seems not need it.
  iw_print_timeval(buffer, &recv_time);
#endif

  iw_init_event_stream(&stream, data, len);
  do
    {
      /* Extract an event and print it */
      ret = iw_extract_event_stream(&stream, &iwe);
      if(ret != 0)
	{
	  if(ret > 0)
	    print_event_token(&iwe, ifname, NULL, 0);
	  else
	   	DBGP(LS_NETWORK_DAEMON,"(Invalid event)\n");
	}
    }
  while(ret > 0);

  return(0);
}
#endif	/* WIRELESS_EXT > 13 */

/*********************** RTNETLINK EVENT DUMP***********************/
/*
 * Dump the events we receive from rtnetlink
 * This code is mostly from Casey
 */

/*------------------------------------------------------------------*/
/*
 * Respond to a single RTM_NEWLINK event from the rtnetlink socket.
 */
static inline int
index2name(int	index, char *name)
{
  int		skfd = -1;	/* generic raw socket desc.	*/
  struct ifreq	irq;
  int		ret = 0;

  memset(name, 0, IFNAMSIZ + 1);

  /* Create a channel to the NET kernel. */
  if((skfd = iw_sockets_open()) < 0)
    {
      perror("socket");
      exit(-1);
    }

  /* Get interface name */
  irq.ifr_ifindex = index;
  if(ioctl(skfd, SIOCGIFNAME, &irq) < 0)
    ret = -1;
  else
    strncpy(name, irq.ifr_name, IFNAMSIZ);

  close(skfd);
  return(ret);
}


/*------------------------------------------------------------------*/
/*
 * Respond to a single RTM_NEWLINK event from the rtnetlink socket.
 */
static int
LinkCatcher(struct nlmsghdr *nlh)
{
  struct ifinfomsg* ifi;
  char ifname[IFNAMSIZ + 1];

#if 0
 	DBGP(LS_NETWORK_DAEMON, "nlmsg_type = %d.\n", nlh->nlmsg_type);
#endif

  if(nlh->nlmsg_type != RTM_NEWLINK)
    return 0;

  ifi = NLMSG_DATA(nlh);

  /* Get a name... */
  index2name(ifi->ifi_index, ifname);

#if WIRELESS_EXT > 13
  /* Code is ugly, but sort of works - Jean II */

  /* Check for attributes */
  if (nlh->nlmsg_len > NLMSG_ALIGN(sizeof(struct ifinfomsg))) {
      int attrlen = nlh->nlmsg_len - NLMSG_ALIGN(sizeof(struct ifinfomsg));
      struct rtattr *attr = (void*)ifi + NLMSG_ALIGN(sizeof(struct ifinfomsg));

      while (RTA_OK(attr, attrlen)) {
	/* Check if the Wireless kind */
	if(attr->rta_type == IFLA_WIRELESS) {
	  /* Go to display it */
	  print_event_stream(ifname,
			     (void *)attr + RTA_ALIGN(sizeof(struct rtattr)),
			     attr->rta_len - RTA_ALIGN(sizeof(struct rtattr)));
	}
	attr = RTA_NEXT(attr, attrlen);
      }
  }
#endif	/* WIRELESS_EXT > 13 */

  return 0;
}

/* ---------------------------------------------------------------- */
/*
 * We must watch the rtnelink socket for events.
 * This routine handles those events (i.e., call this when rth.fd
 * is ready to read).
 */
void handle_netlink_events(struct rtnl_handle *	rth)
{
  while(1)
    {
      struct sockaddr_nl sanl;
      socklen_t sanllen;

      struct nlmsghdr *h;
      int amt;
      char buf[8192];

      amt = recvfrom(rth->fd, buf, sizeof(buf), MSG_DONTWAIT, (struct sockaddr*)&sanl, &sanllen);
      if(amt < 0)
	{
	  if(errno != EINTR && errno != EAGAIN)
	    {
	     	DBGP(LS_NETWORK_DAEMON, "%s: error reading netlink: %s.\n",
		      __PRETTY_FUNCTION__, strerror(errno));
	    }
	  return;
	}

      if(amt == 0)
	{
	 	DBGP(LS_NETWORK_DAEMON, "%s: EOF on netlink??\n", __PRETTY_FUNCTION__);
	  return;
	}

      h = (struct nlmsghdr*)buf;
      while(amt >= (int)sizeof(*h))
	{
	  int len = h->nlmsg_len;
	  int l = len - sizeof(*h);

	  if(l < 0 || len > amt)
	    {
	     	DBGP(LS_NETWORK_DAEMON, "%s: malformed netlink message: len=%d\n", __PRETTY_FUNCTION__, len);
	      break;
	    }

	  switch(h->nlmsg_type)
	    {
	    case RTM_NEWLINK:
	      LinkCatcher(h);
	      break;
	    default:
#if 0
	     	DBGP(LS_NETWORK_DAEMON, "%s: got nlmsg of type %#x.\n", __PRETTY_FUNCTION__, h->nlmsg_type);
#endif
	      break;
	    }

	  len = NLMSG_ALIGN(len);
	  amt -= len;
	  h = (struct nlmsghdr*)((char*)h + len);
	}

      if(amt > 0)
	DBGP(LS_NETWORK_DAEMON, "%s: remnant of size %d on netlink\n", __PRETTY_FUNCTION__, amt);
    }
}

#if 0
/**************************** MAIN LOOP ****************************/

/* ---------------------------------------------------------------- */
/*
 * Wait until we get an event
 */
static inline int
wait_for_event(struct rtnl_handle *	rth)
{
#if 0
  struct timeval	tv;	/* Select timeout */
#endif

  /* Forever */
  while(1)
    {
      fd_set		rfds;		/* File descriptors for select */
      int		last_fd;	/* Last fd */
      int		ret;

      /* Guess what ? We must re-generate rfds each time */
      FD_ZERO(&rfds);
      FD_SET(rth->fd, &rfds);
      last_fd = rth->fd;

      /* Wait until something happens */
      ret = select(last_fd + 1, &rfds, NULL, NULL, NULL);

      /* Check if there was an error */
      if(ret < 0)
	{
	  if(errno == EAGAIN || errno == EINTR)
	    continue;
	 	DBGP(LS_NETWORK_DAEMON, "Unhandled signal - exiting...\n");
	  break;
	}

      /* Check if there was a timeout */
      if(ret == 0)
	{
	  continue;
	}

      /* Check for interface discovery events. */
      if(FD_ISSET(rth->fd, &rfds))
	handle_netlink_events(rth);
    }

  return(0);
}

int
main(int	argc,
     char *	argv[])
{
  struct rtnl_handle	rth;
  int opt;

  /* Open netlink channel */
  if(rtnl_open(&rth, RTMGRP_LINK) < 0)
    {
      perror("Can't initialize rtnetlink socket");
      return(1);
    }

  /* Do what we have to do */
  wait_for_event(&rth);

  /* Cleanup - only if you are pedantic */
  rtnl_close(&rth);

  return(0);
}
#endif
