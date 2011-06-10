/* Copyright (C) 2005, REDSonic, inc.
 * Author:  Wills Yin <wills_yin@redsonic.com>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "sap.h"
#include "sdp.h"

#ifdef HAVE_ZLIB_H
#   include <zlib.h>
#endif


#define MAX_SAP_BUFFER          5000

#define MCAST_SAP_VERSION_MASK  0xE0 /* 3 bits for  SAP version*/
#define MCAST_SAP_VERSION_SHIFT 5    /* Right shift 5 bits to get the version */
#define MCAST_SAP_VER0          0    /* Version 0 */

#define MCAST_SAP_BIT_A         0x10 /* Address type: 0 IPv4, 1 IPv6 */
#define MCAST_SAP_BIT_R         0x08 /* Reserved: Must be 0 */
#define MCAST_SAP_BIT_T         0x04 /* Message Type: 0 announcement, 1 deletion */
#define MCAST_SAP_BIT_E         0x02 /* Encryption Bit: 1 payload encrypted */
#define MCAST_SAP_BIT_C         0x01 /* Compressed Bit: 1 payload zlib compressed */

#define IPV4_ADDR_LEN           4
#define IPV6_ADDR_LEN           16

#ifdef HAVE_ZLIB_H
static int Decompress( const unsigned char *src,unsigned char** _dst , int slen);
#endif

sap_session_t*  sap_parse(char* packet, int packet_len)
{
	int addr_len;
	int total, offset, auth_len;
	int            decomp_size;
	char*          decomp_buffer;
	char           *buffer, *p;
	sap_session_t* sap;;

	if (!packet)
		return NULL;

	total = packet_len;
	buffer = packet;

	decomp_size   = -1;
	decomp_buffer = NULL;
	
	offset = 0;    

	if ((sap = malloc(sizeof(sap_session_t))) == NULL)
		return NULL;
	bzero(sap, sizeof(sap_session_t));

	sap->version = buffer[offset]>>MCAST_SAP_VERSION_SHIFT;
	if (sap->version != 1) 
		fprintf(stderr, "Strange SAP version %d found\n", sap->version);

	sap->addrType = buffer[offset] & MCAST_SAP_BIT_A;

	if ( buffer[offset] & MCAST_SAP_BIT_R ) 
		fprintf(stderr, "SAP packet reserved bit incorrectly set\n");

	sap->isDel       = buffer[offset] & MCAST_SAP_BIT_T;

        sap->isEnc       = buffer[offset] & MCAST_SAP_BIT_E;
	if ( sap->isEnc ) {
		fprintf(stderr, "Encrypted SAP packet, unsupported\n");
		goto error_exit;
	}
        sap->isComp      = buffer[offset] & MCAST_SAP_BIT_C;
	offset++;

	auth_len = buffer[offset];
	offset++;

	sap->hashID = (buffer[offset]<< 8) + buffer[offset + 1];
	offset += 2;

	addr_len     = sap->addrType ? IPV6_ADDR_LEN : IPV4_ADDR_LEN;
	//strncpy(&sap->sourceIP[0], &buffer[offset], addr_len);
	if (addr_len == IPV4_ADDR_LEN)
	    sprintf(sap->sourceIP, "%d.%d.%d.%d", buffer[offset], buffer[offset+1],
			            buffer[offset+2], buffer[offset+3]);
	else
	    strncpy(&sap->sourceIP[0], &buffer[offset], addr_len);
	offset += addr_len;

	offset += auth_len * sizeof(int32_t);

	if (sap->isComp) {
#ifdef HAVE_ZLIB_H
		decomp_size = Decompress( buffer + offset, &decomp_buffer,total - offset );
		if (decomp_size < 0 || decomp_size > MAX_SAP_BUFFER) 
			goto error_exit;

		buffer = decomp_buffer;
		total  = decomp_size;
#else
		fprintf(stderr, "Ignoring compressed sap packet\n");
		goto error_exit;
#endif
	} else {
		buffer += offset;   
		total  -= offset;
	}

	p = buffer;
	/* 
	 * Skip payload type without \0 between SAP and SDP 
	 */
	while ( *p != '\0' && (p[0] != 'v' && p[1] != '='))
		p++;
	if (*p == '\0') p++;
	if( p != buffer && strcasecmp( buffer, "application/sdp" ) )
		fprintf(stderr, "Unhandled content type: %s\n", buffer);        

	sap->sdp = sdp_parse(p, total - (p - buffer));

	SAFE_FREE(decomp_buffer);
	return sap;

error_exit:
	SAFE_FREE(sap);
	SAFE_FREE(decomp_buffer);
	return NULL;
}

void  sap_destroy(sap_session_t* sap)
{
	if ( !sap ) return;
	sdp_destroy(sap->sdp);
	free(sap);
}

#ifdef HAVE_ZLIB_H
static int Decompress( const unsigned char *src,unsigned char** _dst, int slen ) 
{
	int result, dstsize, n;
	unsigned char *dst;
	z_stream       d_stream;

	d_stream.zalloc = (alloc_func)0;
	d_stream.zfree  = (free_func)0;
	d_stream.opaque = (voidpf)0;
	result = inflateInit(&d_stream);
	if( result != Z_OK ) {
		printf( "inflateInit() failed. Result: %d\n", result );
		return( -1 );
	}

	d_stream.next_in  = (Bytef *)src;
	d_stream.avail_in = slen;
	n   = 0;
	dst = NULL;
	do {
		n++;
		dst = (unsigned char *)realloc(dst, n * 1000);
		d_stream.next_out  = (Bytef *)&dst[(n - 1) * 1000];
		d_stream.avail_out = 1000;
		result = inflate(&d_stream, Z_NO_FLUSH);
		if( ( result != Z_OK ) && ( result != Z_STREAM_END ) ) {
			printf( "Zlib decompression failed. Result: %d\n", result );
			return( -1 );
		}
	}
	while( ( d_stream.avail_out == 0 ) && ( d_stream.avail_in != 0 ) && 
					( result != Z_STREAM_END ) );

	dstsize = d_stream.total_out;
	inflateEnd( &d_stream );

	*_dst = (unsigned char *)realloc( dst, dstsize );

	return dstsize;
}
#endif
