/* Copyright (C) 2005, REDSonic, inc.
 * Author:  Wills Yin <wills_yin@redsonic.com>
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "sdp.h"

/*
 * This converts string containing a Network Time Protocol timestamp to
 * a time_t:
 */
#define NTP_TIME_DIFFERENCE 2208988800UL
#define NETWORK_TIME_TO_TIME_T(_ntp_time_value_)                      \
	(((_ntp_time_value_) >= NTP_TIME_DIFFERENCE)                  \
		? (time_t) ((_ntp_time_value_) - NTP_TIME_DIFFERENCE) \
		: (time_t) (_ntp_time_value_))

/* 
 * Struct for session-level and media-level "a" fields: 
 * Exap: a=recvonly; a=cat:Action.
 */
typedef struct  sdp_attr_s  {
	char*        field;
	char*        value;
	struct sdp_attr_s * next;
}sdp_attr_t;

typedef struct  sdp_repeat_s {
	long         interval;
	long         act_dur;
	long*        offsets;
	struct sdp_repeat_s* next;
}sdp_repeat_t;

/*
 * A struct for a "t" "r" "z" field.
 */
typedef struct  sdp_time_s {
	time_t       start;
	time_t       end;
	sdp_repeat_t*  repeat;/* The repreat times */
}sdp_time_t;

/*
 * The struct for session-level and media-level "c" fields: 
 */
typedef struct sdp_conn_s {
	/*
	 * A string containing the type of network the session or media is on:
	 */
	char*        network_type;
	/*
	 * A string containing address type of the address the session or media
	 * is coming from:
	 */
	char*        addr_type;
	/*
	 * A string containing a dotted IP address (e.g., "127.0.0.1") of the
	 * machine the session or media is coming from:
	 */
	char*        address;
	/* Time To Live */
	int          ttl;
}sdp_conn_t;

/*
 * Struct for media description "m" fields: 
 */
typedef struct sdp_media_s {
	/* A string containing the media type (e.g., "audio"): */
	char*        type;
	/* The port number of where the media will be sent: */
	int          port;
	/*
	 * A string containing the name of the protocol to be used to deliver
	 * the stream (e.g., "RTP/AVP"):
	 */
	char*        tran_pro;
	/* A string containing the media formats: */
	int          format;
	/* A string containing a description of the media: */
	char*        desc;
	/*
	 * This will store connection information for this specific piece of
	 * media if there was no session-level "c" field or if the connection
	 * used for this media differs from that used by the rest of the
	 * session:
	 */
	sdp_conn_t*  conn;
	/*
	 * This will store a suggested maximum bandwidth limit for this media
	 * if there was no session-level "b" field or if the bandwidth limits
	 * differ for this media differ than those for the entire session
	 */
	int          bandwidth;
	/*
	 * A list of attribute structs containing the media-level
	 * attributes for this piece of media:
	 */
	sdp_attr_t*  attrs;
	
	struct sdp_media_s* next;
}sdp_media_t;

struct  sdp_desc_s {
	/* o= filed */
	char*        user_name;
	char*        session_id;
	char*        network_type;
	char*        addr_type;
	char*        addr;
	/* s= field */
	char*        session_name;
	/* i= field */
	char*        desc;
	/* u= field */
	char*        uri;
	/* e= field */
	char*        email;
	/* p= field */
	char*        phone;
	/* c= field session level */
	sdp_conn_t*  conn;      
	/* b= field */
	int          bandwidth;
	/* t= r= z= playtime field */
	sdp_time_t*  time;
	/* a= field session level */
	sdp_attr_t*  attrs;
	/* m level fields */
	int          num_media;
	sdp_media_t* media;

	sdp_desc_t*  next;
};

static void sdp_conn_free( sdp_conn_t* conn)
{
	if (!conn) return;
	SAFE_FREE(conn->network_type);
	SAFE_FREE(conn->addr_type);
	SAFE_FREE(conn->addr_type);
	SAFE_FREE(conn->address);
	free(conn);
}

static void sdp_attr_free(sdp_attr_t* attr)
{
	if (!attr) return;
	SAFE_FREE(attr->field);
	SAFE_FREE(attr->value);
	free(attr);
}


static void sdp_media_free( sdp_media_t* media)
{
	if (!media) return;
	SAFE_FREE(media->type);
	SAFE_FREE(media->tran_pro);
	SAFE_FREE(media->desc);
	sdp_conn_free(media->conn);
	DESTROY_LIST(media->attrs, sdp_attr_t, sdp_attr_free);
	free(media);
}

static void sdp_repeat_free(sdp_repeat_t*  repeat)
{
	if (!repeat) return;
	SAFE_FREE(repeat->offsets);
	free(repeat);
}


static int sdp_get_line(const char* src, char* dest, int* start, int total)
{
	int i;
	if (!src || !dest) 
	       return -1;	
	for(i = 0; (*start) < total; i++, (*start)++) {
		dest[i] = src[*start];
		if (dest[i] == '\n')
			break;
	}
	*start += 1;

	if (i >= 1 && dest[i-1] == '\r')
		i--;
	dest[i] = '\0';
	return i;
}

static char* sdp_strcspn(char** strLine, char* delim)
{
	char *p, *q;
	if (!(*strLine) || !delim)
		return NULL;
	p = *strLine;
	q = p + strcspn(*strLine, delim);
	if (*q == '\0')
		*strLine = '\0';
	else {
		*q       = '\0';
		*strLine = q + 1;
	}

	return p;
}

static int sdp_owner_create(sdp_desc_t* sdp, char* buf, char* delim)
{
	int   i;
	char *p, *value;
	char *fields[5];

	if (!sdp || !buf || !delim)
		return -1;
	p = buf;
	for (i = 0; i < 5; i++) {
		value = sdp_strcspn(&p, delim);
		if (value)
			fields[i] = strdup(value);
		else 
			fields[i] = NULL;
	}
	sdp->user_name    = fields[0];
	sdp->session_id   = fields[1];
	sdp->network_type = fields[2];
	sdp->addr_type    = fields[3];
	sdp->addr         = fields[4];

	return 0;
}

static int sdp_conn_create(sdp_conn_t** conn, char* buf, char* delim)
{
	int   i;
	char *p, *value;
	char *fields[5];

	if (!buf || !delim)
		return -1;
	p = buf;

	if (!(*conn) && !(*conn = malloc(sizeof(sdp_conn_t))))
		return -1;
	bzero(*conn, sizeof(sdp_conn_t));

	for (i = 0; i< 3; i++) {
		value = sdp_strcspn(&p, delim);
		if (value)
			fields[i] = strdup(value);
		else
			fields[i] = NULL;
	}

	if ((p = strchr(fields[2], '/')) != NULL) {
		*p++ ='\0';
		(*conn)->ttl= atoi(p);
	}
	(*conn)->network_type = fields[0];
	(*conn)->addr_type    = fields[1];
	(*conn)->address      = fields[2];
	return 0;
}

static int sdp_media_create(sdp_media_t** media, char* buf, char* delim)
{
	int   i;
	char *p, *value;
	char *fields[5];

	if (!buf || !delim)
		return -1;
	p = buf;

	if (!(*media) && !(*media = malloc(sizeof(sdp_media_t))))
		return -1;
	bzero(*media, sizeof(sdp_media_t));

	for (i = 0; i< 4; i++) {
		value = sdp_strcspn(&p, delim);
		if (value)
			fields[i] = strdup(value);
		else
			fields[i] = NULL;
	}

	(*media)->type         = fields[0];
	(*media)->port         = atoi(fields[1]);
	(*media)->tran_pro     = fields[2];
	(*media)->format       = atoi(fields[3]);

	if (fields[1] != NULL)
		free(fields[1]);
	if (fields[3] != NULL)
		free(fields[3]);
	return 0;
}

static int sdp_time_create(sdp_time_t**  time, char* buf, char* delim)
{
	int   i;
	char *p, *value;
	char *fields[5];

	if (!buf || !delim)
		return -1;
	p = buf;

	if (!(*time) && !(*time = malloc(sizeof(sdp_time_t))))
		return -1;
	bzero(*time, sizeof(sdp_time_t));

	for (i = 0; i< 2; i++) {
		value = sdp_strcspn(&p, delim);
		if (value)
			fields[i] = strdup(value);
		else
			fields[i] = NULL;
	}

	if (fields[0] != NULL) {
		unsigned long start = strtoul(fields[0], NULL, 10);
		if (start)
			(*time)->start = NETWORK_TIME_TO_TIME_T(start);
		else
			(*time)->start = 0;
		free(fields[0]);
	}

	if (fields[1] != NULL) {
		unsigned long end = strtoul(fields[1], NULL, 10);
		if (end)
			(*time)->end = NETWORK_TIME_TO_TIME_T(end);
		else
			(*time)->end = 0;
		free(fields[1]);
	}

	return 0;
}

static int sdp_attr_create(sdp_attr_t**  attr, char* buf, char* delim)
{
	int   i;
	char *p, *value;
	char *fields[5];

	if (!buf || !delim)
		return -1;
	p = buf;

	if (!(*attr) && !(*attr = malloc(sizeof(sdp_attr_t))))
		return -1;
	bzero(*attr, sizeof(sdp_attr_t));

	for (i = 0; i< 2; i++) {
		value = sdp_strcspn(&p, delim);
		if (value)
			fields[i] = strdup(value);
		else
			fields[i] = NULL;
	}

	(*attr)->field  = fields[0];
	(*attr)->value  = fields[1];

	return 0;
}

static sdp_media_t* sdp_get_media(sdp_desc_t* sdp, int index)
{
	int i;
	sdp_media_t *m;
	if (!sdp || index >= sdp->num_media)
		return NULL;
	for (i = 0, m = sdp->media; 
			m && i < index;
			i++, m = m->next);
	return m;
}

void sdp_destroy(sdp_desc_t* sdp)
{
	sdp_desc_t *p, *q;
	if (sdp == NULL) return;

	p = sdp;
	while (p) {
	    q = p->next;
	    SAFE_FREE(p->user_name);
	    SAFE_FREE(p->session_id);
	    SAFE_FREE(p->network_type);
	    SAFE_FREE(p->addr_type);
	    SAFE_FREE(p->addr);

	    SAFE_FREE(p->session_name);
	    SAFE_FREE(p->desc);

	    SAFE_FREE(p->uri);
	    SAFE_FREE(p->email);
	    SAFE_FREE(p->phone);
	
	    sdp_conn_free(p->conn);
	    if (p->time) {
		DESTROY_LIST(p->time->repeat, sdp_repeat_t, sdp_repeat_free);
		SAFE_FREE(p->time);
	    }
	    DESTROY_LIST(p->attrs, sdp_attr_t,  sdp_attr_free);
	    DESTROY_LIST(p->media, sdp_media_t, sdp_media_free);
	    free(p);

	    p = q;
	}
}

sdp_desc_t* sdp_parse(const char* payload, int total)
{
	char  line[1024];
	int   linelen, offset, next_offset;
	int   count = 0;
	int   session_level = 1;
	sdp_desc_t*   sdp;
	sdp_media_t*  media = NULL;

	if (payload == NULL) 
		return NULL;
	if ( (sdp = malloc(sizeof(sdp_desc_t))) == NULL) 
		return NULL;
	bzero(sdp, sizeof(sdp_desc_t));

	if (payload[0] != 'v' || payload[1] != '=') {
	    fprintf(stderr,  "SDP bad packet\n");        
	    goto error_exit;
	}

	next_offset =  offset = 0;

	while ( offset < total) {
		char  delim, type;
		char *buf;
		/*
		 * Find the end of the line.
		 */
		linelen = sdp_get_line(payload, line, &next_offset, total);

		/*
		 * Line must contain at least e.g. "v=".
		 */
		buf = line;
		if (linelen < 2) break;
		type  = line[0];
		delim = line[1];
		if (delim != '=') {
			offset = next_offset;
			continue;
		}
		buf += 2;
		
		/*
		 * Attributes.
		 */
		switch (type) {
		case 'v':
			++count;
			if ( count == 2) {
				sdp->next = sdp_parse(payload + offset, total - offset);
				return sdp;
			}
			break;
		case 'o':
			sdp_owner_create(sdp, buf, " ");
			break;
		case 's':
			if (buf[0] == '\0')
				sdp->session_name = NULL;
			else 
				sdp->session_name = strdup(buf);
			break;
		case 'i':
		{
			char *desc;
			if (buf[0] == '\0')
				desc = NULL;
			else 
				desc = strdup(&buf[0]);
			if (session_level == 1)
				sdp->desc   = desc;
			else 
				media->desc = desc;
			break;
		}
		case 'u':
		case 'e':
		case 'p':
			break;
		case 'c':
			if (session_level == 1) 
				sdp_conn_create(&sdp->conn, buf, " ");
			else 
				sdp_conn_create(&media->conn, buf, " ");
			break;
		case 'b':
			break;
		case 't':
			sdp_time_create(&sdp->time, buf, " ");
			break;
		case 'r':
			break;
		case 'm':
			media = NULL;
			sdp_media_create(&media, buf, " ");
			LINK_INTO_LIST(sdp->media, media, sdp_media_t); 
			sdp->num_media++;
			session_level = 0;
			break;
		case 'k':
			break;
		case 'a':
		{
			sdp_attr_t* attr = NULL;
			sdp_attr_create(&attr, buf, ":");
			if (session_level == 1)
				LINK_INTO_LIST(sdp->attrs, attr, sdp_attr_t);
			else 
				LINK_INTO_LIST(media->attrs, attr, sdp_attr_t);
			break;
		}
		case 'z':
			break;
		default:
			break;
		}
		offset = next_offset;
	}
	return sdp;

error_exit:
	sdp_destroy(sdp);
	return NULL;
}

const char* sdp_get_attr(sdp_desc_t* sdp, const char* key, int index)
{
	sdp_attr_t*    p;
	sdp_media_t*   media; 

	if (!sdp || !key || (index >= sdp->num_media))
		return NULL;
	media = sdp_get_media(sdp, index);
	/*
         * Search order:the the media level -> session level  
         */
	p = sdp->attrs;
	while (p) {
		if (p->field && strcasecmp(p->field, key) == 0)
			return p->value;
		p = p->next;
	}

	p = media->attrs;
	while (p) {
		if (p->field && strcasecmp(p->field, key) == 0)
			return p->value;
		p = p->next;
	}
	return NULL;
}

const char* sdp_get_mediaAddr(sdp_desc_t* sdp, int index)
{
	sdp_media_t*   media; 

	if (!sdp || index >= sdp->num_media)
		return NULL;
	media = sdp_get_media(sdp, index);

	if (media && media->conn) 
		return media->conn->address;
	else if (sdp->conn)
		return sdp->conn->address;
	else
		return NULL;
}

const char* sdp_get_mediaProt(sdp_desc_t* sdp, int index)
{
	sdp_media_t*   media; 

	if (!sdp || index >= sdp->num_media)
		return NULL;
	media = sdp_get_media(sdp, index);
	if (media && media->tran_pro && !strcasecmp(media->tran_pro, "RTP/AVP"))
		return "rtp";
	else
		return NULL;
}

int sdp_get_mediaPort(sdp_desc_t* sdp, int index)
{
	sdp_media_t*   media; 

	if (!sdp || index >= sdp->num_media)
		return -1;
	media = sdp_get_media(sdp, index);

	return media ? media->port : -1;
}

int sdp_get_mediaCodec(sdp_desc_t* sdp, int index)
{
	sdp_media_t*   media; 

	if (!sdp || index >= sdp->num_media)
		return -1;
	media = sdp_get_media(sdp, index);

	return media ? media->format : -1;
}

const char* sdp_get_mediaType(sdp_desc_t* sdp, int index)
{
	sdp_media_t*   media; 

	if (!sdp || index >= sdp->num_media)
		return NULL;
	media = sdp_get_media(sdp, index);

	return media ? media->type : 0;
}

const char* sdp_get_sessionName(sdp_desc_t* sdp)
{
	return sdp ? sdp->session_name : NULL ;
}

const char* sdp_get_mediaDesc(sdp_desc_t* sdp, int index)
{
	sdp_media_t*   media; 

	if (!sdp || index >= sdp->num_media)
		return NULL;
	media = sdp_get_media(sdp, index);
	if (media && media->desc)
		return media->desc;
	else if (sdp->desc)
		return sdp->desc;
	else
		return NULL;
}

time_t sdp_get_startTime(sdp_desc_t* sdp)
{
	if (!sdp || !sdp->time)
		return 0;
	return sdp->time->start;
}

time_t sdp_get_endTime(sdp_desc_t* sdp)
{
	if (!sdp || !sdp->time)
		return 0;
	return sdp->time->end;
}

int         sdp_get_mediaNum(sdp_desc_t* sdp)
{
	return sdp ? sdp->num_media : 0;
}

const char* sdp_get_mediaMimetype(sdp_desc_t* sdp, int index)
{
	return NULL;
}
