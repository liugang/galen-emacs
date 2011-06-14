#ifndef __SHOOTER_SUBTITLE_DOWNLOAD_H
#define __SHOOTER_SUBTITLE_DOWNLOAD_H


#define SHOOTER_SUBTITLE_TMP_FOLDER 		"/tmp/subtitle_tmp_folder"
#define SHOOTER_SUBTITLE_TMP_FILE 		"/tmp/subtitle_tmp_folder/subtitle_tmp_file.gz"

#define SHOOTER_SUBTITLE_FILE_FOLDER        "/tmp/subtitle_file"
#define SHOOTER_SUBTITLE_HTML_FILE	      "/tmp/subtitle_file/subtitle.html"
#define SHOOTER_SUBTITLE_HASH_FILE	      "/tmp/subtitle_file/subtitle.txt"
#define SHOOTER_SUBTITLE_RAR_FILE        "/tmp/subtitle_file/subtitle_file.rar"

#define SHOOTER_MAX_PATH_LEN 		512

typedef enum{
	THEME_DL_STATUS_REMOTE_NO_EXISTS_FAIL = -9,
	THEME_DL_STATUS_DEST_IS_NULL_FAIL = -8,
	THEME_DL_STATUS_EXTRACT_FAIL = -7,
	THEME_DL_STATUS_DEST_WRITE_FAIL = -6,
	THEME_DL_STATUS_STORAGE_FULL = -5,
	THEME_DL_STATUS_DEST_OPEN_FAIL = -4,
	THEME_DL_STATUS_READ_ONLY = -3,
	THEME_DL_STATUS_ABORT = -2,
	THEME_DL_STATUS_OTHER_ERROR = -1,
	THEME_DL_STATUS_NONE = 0,
	THEME_DL_STATUS_DONE = 1,
	THEME_DL_STATUS_EXTRACT = 2,
	THEME_DL_STATUS_EXTRACT_SUCCESS = 3,
}THEME_DL_STATUS;

typedef enum{
	SUB_DL_FLAG_INIT=0,
	SUB_DL_FLAG_GET_SUBTITLE_HTML_FILE=1,
	SUB_DL_FLAG_GET_SUBTITLE_HASH_FILE=2,
	SUB_DL_FLAG_GET_SUBTITLE_RAR_FILE=3,
	SUB_DL_FLAG_HAS_GET_SUBTITLE_FILE=4,
	SUB_DL_FLAG_OTHER,
}SHOOTER_SUB_DL_FLAG;

typedef struct {
	pthread_t pid;
	int fd;
	char * url;
	char * dest; //download theme to here.
	char * userpwd;//remote server request username, passwd
	long long size;
	int err;
	THEME_DL_STATUS status;	//download status
	int download_process;
	SHOOTER_SUB_DL_FLAG download_flag;
	char video_full_path[SHOOTER_MAX_PATH_LEN];
} DownloadData;

DownloadData shooterSubDownloadData;

void shooter_subtitle_download_fetch_subfile_by_video_filepath(char * fnVideoFilePath,char * szSubArray,char *szStatMsg,char * szLang);
int shooter_subtitle_download_use_web_api_method(const char * video_file_path);
#endif




