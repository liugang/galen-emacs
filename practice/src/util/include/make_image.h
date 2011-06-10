#ifndef  __MAKE_IMAGE_H__
#define  __MAKE_IMAGE_H__
void	output_gif( char *filename, int w, int h, unsigned char *src, unsigned char *srca, int stride );
void	output_bmp( char *buf, unsigned long size, int w, int h, unsigned char *src, unsigned char *srca, int stride );
void make_png32(unsigned char *buffer, char *filename, 
		int width, int height, int interlace);
int make_jpeg32(unsigned char* buffer, char * filename, int w, int h, int quality);
#endif
