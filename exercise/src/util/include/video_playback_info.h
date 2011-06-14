#ifndef __PLAYBACK_INFOR_H
#define __PLAYBACK_INFOR_H

#include "MPL.h"

typedef enum {
    E_VIDEO_INFO_FILE,
    E_VIDEO_INFO_AUDIO,
    E_VIDEO_INFO_VIDEO,
}ENUM_VIDEO_INFO_TYPE;

typedef struct type2str_s{
	int type;
	const char *str;
}type2str_t;

extern type2str_t video_codec_a[];
extern type2str_t audio_codec_a[];
extern type2str_t file_format_a[];

/* file information */
typedef struct video_info_f_s{
    char *url;
    enum MPL_FILE_FORMAT_TYPE ftype;  /* file format */
}video_info_f_t;

/* audio codec information */
typedef struct video_info_a_s{
    enum MPL_AUDIO_CODEC_TYPE acodec; /* audio codec */
}video_info_a_t;

/* video codec infomation */
typedef struct video_info_v_s{
    enum MPL_VIDEO_CODEC_TYPE vcodec; /* video codec */
    int width;
    int height;
    int bitrate;
}video_info_v_t;

const char *video_get_type_string(type2str_t a[], int type);

/* create a 'video_info_t' object instance */
void* video_playback_info_new(ENUM_VIDEO_INFO_TYPE type);

/* to free 'video_info_t' instance */
int video_playback_info_free(void *info, ENUM_VIDEO_INFO_TYPE type);

/* to save the video info object */
int video_playback_info_save(void *info, ENUM_VIDEO_INFO_TYPE type);

/* to get the video info object */
int video_playback_info_get(void *info, ENUM_VIDEO_INFO_TYPE type);
#endif
