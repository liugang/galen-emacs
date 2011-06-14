/* http://www.geekpage.jp/en/programming/linux-network/ */
/* gai_strerror : getaddrinfo error handling */

/* Like gethostbyname, getaddrinfo also has an error analysis function. This
 * document shows you how to specify the getaddrinfo error using
 * gai_strerror.  */

/* The following example calls getaddrinfo using an invalid argument.

 */


#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int
main()
{
	int err;

	if ((err = getaddrinfo(NULL, NULL, NULL, NULL)) != 0) {
		printf("error %d : %s\n", err, gai_strerror(err));
		return 1;
	}

	return 0;
}


/* Since the argument given to getaddrinfo is invalid, the variable "err" will
 * not be 0 in this example. After getaddrinfo fails, gai_strerror used in
 * printf will show the error desciption.

 * In my environment, this example showed the following error string.
 * error -2 : Name or service not known
 */
