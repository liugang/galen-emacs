#ifndef __AUTO_TRANSCODING__
#define __AUTO_TRANSCODING__

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// simplizied chinese
#define CHARDET_NAME_SIMPLIFIED_CHINESE_GBK     "GBK"
#define CHARDET_NAME_SIMPLIFIED_CHINESE_GB18030 "GB18030"
#define CHARDET_NAME_SIMPLIFIED_CHINESE_GB2312  "GB2312"
#define CHARDET_NAME_SIMPLIFIED_CHINESE_HZ      "HZ"
#define CHARDET_NAME_SIMPLIFIED_CHINESE_ISO2022CN "ISO-2022-CN"

// traditional chinese
#define CHARDET_NAME_TRADITIONAL_CHINESE_BIG5        "Big5"
#define CHARDET_NAME_TRADITIONAL_CHINESE_BIG5HKSCS   "Big5-HKSCS"
#define CHARDET_NAME_TRADITIONAL_CHINESE_EUCTW       "EUC-TW"

// japanese
#define CHARDET_NAME_JAPANESE_SHIFT_JIS          "SHIFT_JIS"
#define CHARDET_NAME_JAPANESE_EUCJP              "EUC-JP"
#define CHARDET_NAME_JAPANESE_ISO2022JP          "ISO-2022-JP"

// korean
#define CHARDET_NAME_KOREAN_EUCKR                "EUC-KR"
#define CHARDET_NAME_KOREAN_ISO2022KR            "ISO-2022-KR"
#define CHARDET_NAME_KOREAN_JOHAB                "JOHAB"
#define CHARDET_NAME_KOREAN_UHC                  "UHC"

// western
#define CHARDET_NAME_WESTERN_IBM850              "IBM-850"
#define CHARDET_NAME_WESTERN_ISO88591            "ISO-8859-1"
#define CHARDET_NAME_WESTERN_MACROMAN            "MACROMAN"
#define CHARDET_NAME_WESTERN_WINDOWS1252         "WINDOWS-1252"

#define CHARDET_NAME_WINDOWS1254                 "WINDOWS-1254"
#define CHARDET_NAME_GREEK                       "ISO-8859-7"
#define CHARDET_NAME_SOUTH_EUROPEAN              "ISO-8859-3"
#define CHARDET_NAME_NORTH_EUROPEAN              "ISO-8859-4"
#define CHARDET_NAME_ISO_8859_6                  "ISO-8859-6"
#define CHARDET_NAME_ISO_8859_8                  "ISO-8859-8"

// central european
#define CHARDET_NAME_CENTRAL_EUROPEAN_IBM852     "IBM-852"
#define CHARDET_NAME_CENTRAL_EUROPEAN_ISO88592   "ISO-8859-2"
#define CHARDET_NAME_CENTRAL_EUROPEAN_MACCE      "MACCE"
#define CHARDET_NAME_CENTRAL_EUROPEAN_WINDOWS1250 "WINDOWS-1250"

// russian
#define CHARDET_NAME_RUSSIAN_CP866 "CP866"

// unicode
#define CHARDET_NAME_UTF8                       "UTF-8"
#define CHARDET_NAME_UTF16_LE                   "UTF-16LE"
#define CHARDET_NAME_UTF16_BE                   "UTF-16BE"
#define CHARDET_NAME_ASCII                      "ASCII"
#define CHARDET_NAME_ANSI                       "MS-ANSI"


char *transcode_get_locale_encoding_type(const char *locale);

/**
 * Functionality: 
 *  Convert input buffer to utf-8 encoding data.
 * Parameters: 
 *  srcstr, source data need to be converted
 *  srcstr_len, specify the length of source data
 * Return:
 *  if conversion is successful, return value is the utf-8 encoding data for the input buffer.
 *  if there is an error, NULL will be returned.
 * Notice: 
 *  1. You must call free() to release the memory which has been allocated for the return value.
 *  2. Sometimes the output utf-8 encoding data starts with "0xEF,0xBB,0xBF", we call these bytes as BOM(byte order mark).
 *  Since our engine could not show utf-8 encoding data correctly if BOM exists, the BOM will be removed internally then return to user.
 *  More details please refer to document(http://en.wikipedia.org/wiki/UTF-8, at "Byte-order mark" section).
 *  Or we could add another parameter to implicitly indicates whether or not should we remove the BOM.
 **/
char *transcode_convert_to_utf8(const char *srcstr, 
		size_t srcstr_len);

char *transcode_convert_to_utf8_v2(const char *srcstr, 
		size_t srcstr_len, 
		const char *src_encoding);

/**
 * Functionallity: 
 *  Converts input file to utf-8 encoding and stores the results as a new file.
 * Parameters: 
 *  srcfile, the file to be converted
 *  destfile, the result file contains utf-8 encoding data.
 * Return:
 *  The return value indicates how many data has been stored in destfile
 *  if there is an error, -1 will be returned.
 */
int transcode_convert_file_to_utf8_file(const char *srcfile, 
		const char *destfile);

int transcode_convert_file_to_utf8_file_v2(const char *srcfile, 
		const char *destfile, 
		const char *src_encoding);

#endif


