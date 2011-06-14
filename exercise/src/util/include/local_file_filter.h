#ifndef __LOCAL_FILE_FILTER__H
#define __LOCAL_FILE_FILTER__H

struct local_file_media_info {
	char file_extension[128];
	char mime_type[128];
	char protinfo[128];
};

extern struct local_file_media_info local_media_info_audio[];
extern struct local_file_media_info local_media_info_image[];
extern struct local_file_media_info local_media_info_video[]; 
extern struct local_file_media_info local_media_info_bt_seed[]; 
extern struct local_file_media_info local_media_info_playlist[]; 
extern struct local_file_media_info local_media_info_video_playlist[]; 

#define LOCAL_FILTER_FILE_ALL			0xffffffff
#define LOCAL_FILTER_FILE_CONTAINER		0x00000001
#define LOCAL_FILTER_FILE_VIDEO			0x00000002
#define LOCAL_FILTER_FILE_AUDIO			0x00000004
#define LOCAL_FILTER_FILE_IMAGE			0x00000008
#define LOCAL_FILTER_FILE_BT_SEED		0x00000010
#define LOCAL_FILTER_FILE_PLAYLIST		0x00000020
#define LOCAL_FILTER_FILE_DOWNLOADLIST		0x00000040
#define LOCAL_FILTER_FILE_DOWNLOAD_LINK		0x00000080
#define LOCAL_FILTER_FILE_VIDEO_PLAYLIST		0x00000100

#define STR_FILTER_ALL                  "*"
#define STR_FILTER_VIDEO                "video:"
#define STR_FILTER_AUDIO                "audio:"
#define STR_FILTER_IMAGE                "image:"

#define LOCAL_FILTER_FILE_SYSTEM_ALL		0xffffffff
#define LOCAL_FILTER_FILE_SYSTEM_NONE		0x00000000
#define LOCAL_FILTER_FILE_SYSTEM_FAT		0x00000001
#define LOCAL_FILTER_FILE_SYSTEM_NTFS		0x00000002
#define LOCAL_FILTER_FILE_SYSTEM_HFS		0x00000004

#define PLAYLIST_MPLS_MIME_TYPE	"playlist/mpls"
#define PLAYLIST_MPLS_PROT_INFO	"file-get:*:playlist/mpls:*"

enum lcb_file_type{
	FILE_TYPE_UNKNOW,
	FILE_CONTAINER,
	FILE_PLAYLIST,
	FILE_DOWNLOADLIST,
	FILE_AUDIO,
	FILE_IMAGE,
	FILE_VIDEO,
	FILE_BT_SEED,
	FILE_DOWNLOAD_ED2K_LINK,
#ifdef CONF_THUNDER
	FILE_DOWNLOAD_THUNDER_LINK,
#endif
	FILE_DOWNLOAD_HTTP_LINK,
#ifdef CONF_ENABLE_BLURAY_NAVIGATION
	FILE_VIDEO_PLAYLIST
#endif

};

struct local_file_data
{
	enum lcb_file_type file_type;
	unsigned short index;	
	off_t size;
	unsigned int duration;
	time_t atime;
	time_t mtime;
	time_t ctime;
};

int local_filter_string2type(const char *filterstring);
int local_file_filter(char *file, unsigned int filter_type, struct local_file_data *file_data, unsigned char d_type);
int local_file_filter_with_file_system(char *file, unsigned int filter_type, struct local_file_data *file_data, unsigned int file_system, unsigned char d_type);
int local_file_is_visible_with_file_system(char *file,unsigned int mask_file_system);
int local_file_is_visible(char *file);

#endif
