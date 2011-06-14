#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "insane.h"

int main(int argc, char **argv)
{
    char *ifname = "insane";
    char *prgname = argv[0];
    char *s;
    int sock = socket(AF_INET, SOCK_RAW, IPPROTO_RAW);
    struct insane_userinfo info;
    struct ifreq req;

    if (sock < 0) {
        fprintf(stderr, "%s: socket(): %s\n", prgname, strerror(errno));
        exit(1);
    }

    /* prepare the fields of this interface request */
    strcpy(req.ifr_name, ifname);

    /* check that the insane interface exists */
    if ( ioctl(sock, SIOCGIFFLAGS, &req) < 0 ) {
	fprintf(stderr, "%s: %s: %s", prgname, ifname, strerror(errno));
	if (errno == ENODEV)
	    fprintf(stderr, " (did you load the module?)");
	putc('\n', stderr);
	exit(1);
    }

    switch(argc) {
	case 1: /* get information */
	    req.ifr_data = (caddr_t)&info;
	    if (ioctl(sock, SIOCINSANEGETINFO, &req)<0) {
		fprintf(stderr, "%s: ioctl(INSANEGETINFO): %s\n", prgname,
			strerror(errno));
		exit(1);
	    }
	    printf("slave:     %s\n", info.name);
	    printf("mode:      %s\n", info.mode == INSANE_PERCENT ? "percent" :
		   (info.mode == INSANE_TIME ? "time" : "pass"));
	    printf("arguments: %i %i\n", info.arg1, info.arg2);
	    break;

        case 3:
        case 4: 
        case 5:

	    /* The first argument is the slave, then the mode, then args */
	    req.ifr_data = (caddr_t)&info;
	    strncpy(info.name, argv[1], INSANE_NAMELEN);
	    info.arg1 = info.arg2 = 0;
	    if (!strcasecmp(argv[2],"pass")) {
		info.mode = INSANE_PASS;
		if (argc != 3) {
		    fprintf(stderr, "%s: Wrong number of arguments\n",prgname);
		    exit(1);
		}
	    } else if (!strcasecmp(argv[2],"percent")) {
		info.mode = INSANE_PERCENT;
		if (argc != 4) {
		    fprintf(stderr, "%s: Wrong number of arguments\n",prgname);
		    exit(1);
		}
		info.arg1 = (int)strtol(argv[3], &s, 0);
		if (*s || info.arg1 < 0 || info.arg2 > 100) {
		    fprintf(stderr, "%s: Wrong value \"%s\"\n",
			    prgname, argv[3]);
		    exit(1);
		}
	    } else if (!strcasecmp(argv[2],"time")) {
		info.mode = INSANE_TIME;
		if (argc != 5) {
		    fprintf(stderr, "%s: Wrong number of arguments\n",prgname);
		    exit(1);
		}
		info.arg1 = (int)strtol(argv[3], &s, 0);
		if (*s || info.arg1 < 0) {
		    fprintf(stderr, "%s: Wrong value \"%s\"\n",
			    prgname, argv[3]);
		    exit(1);
		}
		info.arg2 = (int)strtol(argv[4], &s, 0);
		if (*s || info.arg2 < 0) {
		    fprintf(stderr, "%s: Wrong value \"%s\"\n",
			    prgname, argv[4]);
		    exit(1);
		}
	    } else {
		fprintf(stderr, "%s: Wrong mode \"%s\"\n", prgname, argv[2]);
	    }

	    /* ok, now pass this information to the device */
	    if (ioctl(sock, SIOCINSANESETINFO, &req)<0) {
		fprintf(stderr, "%s: ioctl(INSANESETINFO): %s\n", prgname,
			strerror(errno));
		exit(1);
	    }
	    break;

        default:
	    fprintf(stderr, "%s: Wrong number of arguments\n", prgname);
	    exit(1);
    }
    return 0;
}
