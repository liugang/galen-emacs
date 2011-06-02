#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>
#include <fcntl.h>
#include <pthread.h>
#include <semaphore.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <asm/types.h>
#include <linux/if.h>
#include <linux/wireless.h>
#include <arpa/inet.h>
#include <errno.h>

/*
 * Open a socket.
 * Depending on the protocol present, open the right socket. The socket
 * will allow us to talk to the driver.
 */
int
iw_sockets_open(void)
{
  static const int families[] = {
    AF_INET, AF_IPX, AF_AX25, AF_APPLETALK
  };
  unsigned int	i;
  int		sock;

  /*
   * Now pick any (exisiting) useful socket family for generic queries
   *
   * Note : don't open all the socket, only returns when one matches,
   * all protocols might not be valid.
   * Workaround by Jim Kaba <jkaba@sarnoff.com>
   * Note : in 99% of the case, we will just open the inet_sock.
   * The remaining 1% case are not fully correct...
   */

  /* Try all families we support */
  for(i = 0; i < sizeof(families)/sizeof(int); ++i)
    {
      /* Try to open the socket, if success returns it */
      sock = socket(families[i], SOCK_DGRAM, 0);
      if(sock >= 0)
	return sock;
  }

  return -1;
}

int main(int argc, char *argv[])
{
	int skfd;

	if ((skfd = iw_sockets_open()) < 0) {
		printf("(%s:%d) iw_sockets_open() fail.\n", __FUNCTION__, __LINE__);
		return -1;
	}

	printf("======	skfd = %d ======\n", skfd);
//	if(is_wifi_direct) {
//		ret = set_private(skfd, (char *)wlan->wlan_p2p_name, "stat", (void *)&data, arg_count, NULL);
//	} else {
//		ret = set_private(skfd, (char *)wlan->wlan_name, "stat", (void *)&data, arg_count, NULL);
//	}
//	if (ret == -1) {
//		fprintf(stderr, "wrong stat ioctl command\n");
//		close(skfd);
//		return -1;
//	}
//

	close(skfd);

	return 0;
}
