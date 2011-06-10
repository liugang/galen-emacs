#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mkconfig_api.h"

int main(int argc, char **argv)
{
    char product[32]={0};
    char version[32]={0};
    char *str=NULL;

    if (argc!=2){
	fprintf(stdout, "usage: %s <config file>\n", argv[0]);
	exit(0);
    }
    str = strdup(argv[1]);
    mkconfig_translate_file_to_config_dir(str, product, version);
    exit(0);
}

