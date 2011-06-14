#ifndef __SUBTITLE_API_H
#define __SUBTITLE_API_H

#define NUMBER_SUBFILE 64
#define SUB_NUM_ONE_FILE 16
#define LENGTH_SUBFILE_NAME 50
#define NUM_SUB_SHOW 5
#define SUBTITLE_HEIGHT_OFFSET 140
#define SUBTITLE_PREFIX "/tmp/subtitle-"
#define CONF_SUBTITLE_MOVE_STEP 	10
#define CONF_SUBTITLE_POSITON_NUM       30

typedef enum ENUM_MEDIA_TYPE_s {
	MEDIA_TYPE_AUDIO = 0,
	MEDIA_TYPE_VIDEO,
} ENUM_MEDIA_TYPE;


typedef	enum subtitle_attribute_e {
	SUBTITLE_ATTRIBUTE_NONE,
	SUBTITLE_ATTRIBUTE_STRING,
	SUBTITLE_ATTRIBUTE_PICTURE,
} subtitle_attribute_t;

extern int subtitle_fontsize[7];

typedef struct {
	long        	 time_on;
	long        	 time_off;
	char        	*text[2];
	int          	fillpos;

	subtitle_attribute_t	attrib;
	/*
	 * Below parameters MUST BE filled if the attrib is SUBTITLE_ATTRIBUTE_PICTURE
	 */ 
	int		x,y,w,h;	
	int		frame_width;
	int		frame_height;
	char		*pvob_buf;
	unsigned long int	vob_size;
} subtitle_content_t;

struct	subtitle_plugin_s; 
typedef struct {
	subtitle_content_t	*content;
	int           curpos;
	int           total;
	void		*priv;
	struct	subtitle_plugin_s	*ref_plugin;
} subtitle_object_t;

typedef enum ENUM_SUBTITLE_TYPE_s {
	SUBTITLE_TYPE_NONE = 0,
	SUBTITLE_TYPE_SRT,
	SUBTITLE_TYPE_SUB,
	SUBTITLE_TYPE_SMI,
	SUBTITLE_TYPE_SSA,
	SUBTITLE_TYPE_ASS,
	SUBTITLE_TYPE_SUBVIEWER,
	SUBTITLE_TYPE_SUBVIEWER2,
	SUBTITLE_TYPE_LRC,
	SUBTITLE_TYPE_VOBSUB,
	SUBTITLE_TYPE_TXT,
	SUBTITLE_TYPE_XSUB_DXSA,
	SUBTITLE_TYPE_XSUB_DXSB,
	SUBTITLE_TYPE_EMBEDDED_VOBSUB,
	SUBTITLE_TYPE_EMBEDDED_TEXT,
}ENUM_SUBTITLE_TYPE;

typedef enum {
	SUBTITLE_OK = 0,
	SUBTITLE_INEXISTENCE,
	SUBTITLE_CAN_NOT_OPEN,
	SUBTITLE_NOT_SUPPORTED,  /*the subtitle can not transcoding to UTF8*/
	SUBTITLE_ERROR,
	SUBTITLE_INSIDE,            /*the subtitle parse from the player*/
}ENUM_SUBTITLE_STATE;

typedef struct {
	ENUM_MEDIA_TYPE		media_type;
	ENUM_SUBTITLE_TYPE	subtitle_type;
} subtitle_desc_t;

typedef struct subtitle_plugin_s {
	int type;
	int media_type;
	subtitle_object_t 	*(*set)(const char *subfilepath,int id, char *encoding);
	int 			(*check)(char *full_subtitle_path,const char *ext,FILE *fp, int *ptr_num_subtitles,char *locale[],char *language[]);
	subtitle_content_t 	*(*parse)(subtitle_object_t *object, long time);
	void			(*destroy)(subtitle_object_t *object );

}subtitle_plugin_t;

typedef struct subtitle_vobsub_dec_data_s {
	char		*pbmp_buf;
	int		bmp_size;
	int		x,y,w,h;
	char		*pvob_buf;
	unsigned long int	vob_size;
	unsigned int palette[16];
} subtitle_vobsub_dec_data_t;

void *vobsub_dec_open(unsigned int *palette, unsigned int *cuspal, unsigned int custom, unsigned int frame_width, unsigned int frame_height);
void vobsub_dec_assemble(void *this, unsigned char *packet, unsigned int len, long pts_ms);
void vobsub_dec_close(void *this);
void vobsub_dec_get(void *this,subtitle_vobsub_dec_data_t *subtitle);
subtitle_vobsub_dec_data_t * subtitle_vobsub_dec_data_init(int w, int h);
void subtitle_vobsub_dec_data_uninit(subtitle_vobsub_dec_data_t *subtitle);
char *Subtitle_GetLanguageName(char *code);
subtitle_vobsub_dec_data_t *xsub_dec_init(unsigned char *data, int data_size,ENUM_SUBTITLE_TYPE subtitle_type);
void xsub_dec_close(subtitle_vobsub_dec_data_t *subtitle);

/*
 * External API
 */
void	subtitle_plugin_factory(void);
int subtitle_get_count(subtitle_desc_t *desc, char *fullpath, int player_subtitle_num);
int subtitle_get_count_for_download_subtitle(subtitle_desc_t *desc, char *fullpath, int player_subtitle_num,char * subtitle_fullpath);
int  subtitle_set(int id, ENUM_SUBTITLE_TYPE * subtitle_type, char **subtitle_language, int chang_encoding, char *encoding_type);
subtitle_content_t	*subtitle_display(long time);
void	subtitle_reset(void);
void subtitle_files_clear();
char* embeded_subtitle_get_language(char *language,int language_length);
char* external_subtitle_get_language(int subtitle_num);
unsigned int get_subtitle_color(int color_index);
ENUM_SUBTITLE_TYPE embeded_subtitle_get_type(int embeded_subtitle_type);
/*
 * Internal API
 */
void	subtitle_object_destroy(subtitle_object_t **object);
char	*subtitle_ignore_line_break(char *text);
void transcode_subtitle_content_to_utf8(char **dest1,char **dest2,char *encoding);
char *transcode_file_as_utf16(const char *filename);
subtitle_content_t	*subtitle_get_subtitle_content_by_time(subtitle_object_t *object, long time);
subtitle_content_t *subtitle_get_next_subtitle(subtitle_content_t *cur_subtitle);
subtitle_content_t *subtitle_get_pre_subtitle(subtitle_content_t *cur_subtitle);
char *subtitle_skip_tag(char *buf);
unsigned int vobsub_argb_to_avyu(unsigned int rgb);

#endif
