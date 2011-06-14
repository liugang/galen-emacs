#ifndef __DIRECTORY_H__
#define __DIRECTORY_H__

extern int OS_touch(const char *path);
extern int OS_GetDirectoryChildCount(const char *path);
extern int OS_fileexist(const char *name);
extern void OS_SaveConfig();
extern int OS_create_path(const char *path);

#endif//__DIRECTORY_H__
