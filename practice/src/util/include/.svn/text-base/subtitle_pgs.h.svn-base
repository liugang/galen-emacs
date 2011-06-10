#ifndef __SUBTITLE_PGS_H
#define __SUBTITLE_PGS_H
#include "mpegts.h"

typedef struct PGSSubPresentation {
	unsigned int x; 
	unsigned int y; 
	unsigned int video_w; 
	unsigned int video_h; 
	unsigned int id_number; 
	int64_t start_pts;
} PGSSubPresentation; 

typedef struct PGSSubPicture { 
 	unsigned int w; 
	unsigned int h;
	unsigned int stride;
	uint8_t *bitmap; 
	uint8_t *abitmap; 
	unsigned long bitmap_size;
	unsigned char *buf;
	unsigned long buf_size;
} PGSSubPicture; 

typedef struct RLEData {
	uint8_t *pre_RLEdata;
	unsigned long stored_data_len;
	unsigned int rle_bitmap_len;
} RLEData;

typedef struct PGSPalette {
	unsigned long int clut[256];
	unsigned char *buf;
	int buf_size;
} PGSPalette;

typedef struct PGSSubContext { 
 	PGSSubPresentation presentation; 
	PGSPalette palette;
	unsigned long int clut[256];
	PGSSubPicture picture; 
	RLEData rle_data;
	int last_segment_type;
	unsigned long need_size;
} PGSSubContext; 

void parse_pes_stream(unsigned char *pBuffer1,unsigned long size1,
                      void (*cb)(int id,int w,int h,int x,int y,unsigned char *data,int size,long long int pts),int idx,int video_w,int video_h);
void PGS_Context_Free(); 
#endif
