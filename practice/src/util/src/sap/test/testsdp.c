/* Copyright (C) 2005, REDSonic, inc.
 * Author:  Wills Yin <wills_yin@redsonic.com>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>
#include "sdp.h"

static int testLoadFile(const char* filename, char** buffer, int* size)
{
	FILE*        fp;
	struct stat  info;

	if (!filename) 
		return -1;

	if (stat(filename, &info) || ((fp = fopen(filename, "rb")) == NULL)) {
		fprintf(stderr, "ERROR: cannot open file : %s\n", strerror(errno));
		return -1;
	}

	if ( ((*buffer = malloc(info.st_size)) == NULL)  || 
			(fread(*buffer, info.st_size, 1, fp) != 1))  {
            fprintf(stderr, "%s:%d, ERROR: out of memory\n", __FUNCTION__, __LINE__);
	    if (*buffer) {
		    free(*buffer);
		    *buffer = NULL;
	    }
	}
	*size = info.st_size;
	if (fp) fclose(fp);
	return 0;
}

int main (int argc, char** argv)
{
	int          i, total, num;
	char*        buffer;
	time_t       start, end;
	sdp_desc_t*  sdp;

	if (argc != 2) {
		fprintf(stderr, "Usage: <program> file\n");
		return -1;
	}
	buffer = NULL;
	total  = 0;

	if (testLoadFile(argv[1], &buffer, &total) == -1)
		return -1;
	sdp = sdp_parse(buffer, total );

	fprintf(stderr, "The sdp session level:\n");
	fprintf(stderr, "\tThe sdp session name is: %s\n", 
			sdp_get_sessionName(sdp));
	start = sdp_get_startTime(sdp);
	end   = sdp_get_endTime(sdp);
	num   = sdp_get_mediaNum(sdp);
	if (start && end) {
		fprintf(stderr, "\tThe sdp start time is: %s", ctime(&start));
		fprintf(stderr, "\tThe sdp end time is: %s", ctime(&end));
	} else {
		fprintf(stderr, "\tThe sdp end time is: %ld\n", start);
		fprintf(stderr, "\tThe sdp end time is: %ld\n", end);
	}
	fprintf(stderr, "\tThe sdp session has %d media.\n", num);
	fprintf(stderr, "\n");
	fprintf(stderr, "\n");

	for (i = 0; i < num; i++) {
		fprintf(stderr, "The sdp media level %d:\n", i);
		fprintf(stderr, "\tThe meida %d description is: %s\n", 
				i, sdp_get_mediaDesc(sdp, i));
		fprintf(stderr, "\tThe media %d address is: %s\n", 
				i, sdp_get_mediaAddr(sdp, i));
		fprintf(stderr, "\tThe media %d protocol is: %s\n", 
				i, sdp_get_mediaProt(sdp, i));
		fprintf(stderr, "\tThe media %d type is: %s\n", 
				i, sdp_get_mediaType(sdp, i));
		fprintf(stderr, "\tThe media %d port is: %d\n", 
				i, sdp_get_mediaPort(sdp, i));
		fprintf(stderr, "\tThe media %d codec is: %d\n", 
				i, sdp_get_mediaCodec(sdp, i));
		fprintf(stderr, "\n");
	}
	sdp_destroy(sdp);

	if (buffer) free(buffer);
	return 0;
}
