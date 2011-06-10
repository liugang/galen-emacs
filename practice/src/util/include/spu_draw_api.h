#ifndef _SPU_DRAW_API_H_
#define _SPU_DRAW_API_H_

#include <ft2build.h>
#include FT_FREETYPE_H

#define SPU_SCALER_HEIGHT 720
#define SPU_SCALER_WIDTH 1280
#define SPU_SCALER_HEIGHT_SD 480
#define SPU_SCALER_WIDTH_SD 720
#define SPU_POSITION_X 100
#define SPU_POSITION_Y 525
#define SPU_SUBTITLE_POSITON_TOP_DISTANCE_Y 280 
#define SPU_SUBTITLE_POSITON_BOTTOM_DISTANCE_Y 40 
#define MAX_WORDS_OF_SUBTITLE 2048
/* adjustment the font size if we change the SPU_SCALER_WIDTH and SPU_SCALER_HEIGHT
 * 1920x1080 FONT_SIZE_OFFSET is 26 
 * 1280x720 FONT_SIZE_OFFSET is 0 
 * */
#define FONT_SIZE_OFFSET 0
typedef enum{
	SUBTITLE_ON_SMP86XX_SPU,
	SUBTITLE_ON_SMP86XX_OSD,
	SUBTITLE_ON_SMP86XX_DFB
}spu_plugin_type;

struct spu_hal_s {
	/* open the hal and do necessary initialization job, such as open scaler, setup the window size to default value */
	int (*open)(void *, void **, int, int, unsigned long int *, size_t, int);
	/* draw subtitle buffer on scaler or frame buffer */
	int (*draw)(void *, void *, unsigned char *,unsigned long int,unsigned long int, size_t, int, int);
	/* destroy the necessary resources */
	int (*close)(void *, void*);
	/* set the platette */
	int (*set_palette)(void *, void *, unsigned long int *, size_t);
};

typedef struct spu_hal_s spu_hal_t;

struct spu_handler_s {
	spu_hal_t op;
	int spu_width;
	int spu_height;
	int x; 
	int y;
       	/* 
	 * x_gap: the gap between two words.
	 * y_gap: the gap between two string line.
	 */
	int x_gap; 
	int y_gap;
	int font_size;
	int subtitle_outline;
	FT_Face face;
	FT_Library library;
	int FT_inited;
	unsigned char *bmp_buf;
	unsigned long int  bmp_buf_size;
	unsigned char *line_buf;
	unsigned long int line_buf_size;
	unsigned int line_height;
	unsigned int line_width;
	unsigned long int last_bmp_buf_size;
	void *scaler_data;
	void *scaler_info;
};

typedef struct spu_handler_s spu_handler_t;

spu_handler_t *spu_init(void *_scaler_data, spu_plugin_type pt, int font_size, int width, int height, unsigned int x, unsigned int y, unsigned long int *spu_lut, size_t lut_size, char *font_file, int keycolor_index);
int spu_set(spu_handler_t *handler, unsigned char *str1, int len1, unsigned char *str2, int len2, int keycolor_index);
int spu_set_bitmap(spu_handler_t *handler, char *bmp_buf, int buf_size, int x, int y, int width, int height, int keycolor_index);
int spu_get_default_palette(unsigned long int *palette, size_t size);
int spu_get_font_file(char *store_file, char *font, size_t size);
int spu_uninit(void *_scaler_data, spu_handler_t *handler);
int spu_set_palette(spu_handler_t *handler, unsigned long int *palette, size_t size);

int spu_get_palette(unsigned long int *palette, size_t size, unsigned int bg_color, unsigned int text_color);
int spu_update_position(spu_handler_t *handler, int keycolor_index);
#endif

