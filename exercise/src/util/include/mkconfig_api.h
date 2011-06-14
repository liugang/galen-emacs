#ifndef __MKCONFIG_API_H__
#define __MKCONFIG_API_H__

#define MAX_FULL_PATH_LEN	(512)
#define MAX_NAME_LENGTH		(32)
#define CONFIG_MAGIC_NAME_LENGTH	(64)

#define MAX_FILE_NUM		(20)
#define BUF_SIZE                (1024)
#define CRC_TABLE_INDEX		(256)

typedef struct CONFIG_FILES_HEADER { 
	char magicname[CONFIG_MAGIC_NAME_LENGTH];
	char type; 		/* type of header format, it is used for distinguish with older header format.*/
	char version;		/* version of header format */
	unsigned long crc;
	int filecounts;
	int fullsizes;
	unsigned char reserved[512];
} CONFIG_FILES_HEADER;

typedef struct CONFIG_FILE_INFO { 
	char filename[MAX_FULL_PATH_LEN];
	int size;
} CONFIG_FILE_INFO;

unsigned long update_crc (unsigned long crc, char *buf, int len);
void make_crc_table (void);
unsigned long get_file_crc(const char *file);
int mkconfig_translate_config_dir_to_file (char *i_dir, char *o_filename, char *product, char *version);
int mkconfig_translate_file_to_config_dir (char *i_filename, char *product, char *version);
#ifdef DEBUG
void dump_file_content (char *filename);
#endif
#endif
