/* 
 * Copyright (C) 2007, ALPHA Networks, inc.
 */
#ifndef __CHAPTER_TOOL__
#define __CHAPTER_TOOL__

#define MAX_CHAPTERS                            64
#define MAX_CHAPTER_STRING_LEN                  128

typedef enum {
	CHAPTER_ERROR = -2,
	CHAPTER_PARSED_FAIL = -1,
	CHAPTER_FILE_NOT_FOUND = 0,
	CHAPTER_PARSED_OK,
} chapterParseMsg;

enum {
	CHAPTER_NOT_EXISTS = 0,
	CHAPTER_EXISTS,
};

typedef struct {
        long long       ChapterStart;
        long long       ChapterEnd;
        char            ChapterString[MAX_CHAPTER_STRING_LEN];
} chapterData;

typedef struct {
        int             nChapters;
        int             Cur_Chapter;
       chapterData     ChapterArray[MAX_CHAPTERS];
} chapterListData;

int check_chapter_exist();
int parse_chapter(chapterListData *chapterList);
void print_chapters(chapterListData *chapterList);
int write_chapter(chapterListData *chapterList);

#endif
