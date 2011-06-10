/*
 * vim:cindent:ts=8:
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "mkconfig_api.h"
#include "securesoho.h"
#include "fwup_postprocess.h"
#include "config_tool_event.h"
#include "event_proxy.h"
#include "flash_op.h"

#define CONFIG_TOOL_OP_LOAD         0x00000001
#define CONFIG_TOOL_OP_SAVE         0x00000002
#define CONFIG_TOOL_OP_MONITOR      0x00000004
#define CONFIG_TOOL_OP_SAVEDIR      0x00000008
#define CONFIG_TOOL_OP_REGION       0x00000010
#define CONFIG_TOOL_OP_ERASE_FLASH  0x00000020
#define CONFIG_TOOL_OP_EXTRACT      0x00000040


typedef struct config_post_process_handle_s{
	const char *product;
	void (*post_process)(const char *version);
}config_post_process_handle_t;

void amp150_post_process(const char *version);
void config_post_process(const char *version);

config_post_process_handle_t oem_handle_array[]={
	{"AMP150",  amp150_post_process},
	{NULL, NULL}
};

static config_post_process_handle_t *get_oem_handle(const char *product)
{
	int i=0;
	config_post_process_handle_t *hd=NULL;

	while(oem_handle_array[i].product){
		if(!strcmp(oem_handle_array[i].product, product)){
			hd = &(oem_handle_array[i]);
			goto out;
		}
		i++;
	}
out:
	return hd;
}

void amp150_post_process(const char *version)
{
	char cur_version[32]={0};

	printf("F:%s,%d\n", __FUNCTION__, __LINE__);
	securesoho_get_curversion(cur_version);

	if(NULL==version || strcmp(cur_version, version)){
		/* do post_process when F/W updated */
		fwup_post_process("/conf_src/amp150_config_policy.xml");
	}
}

void config_post_process(const char *version)
{
	char cur_version[32]={0};
	printf("F:%s,%d\n", __FUNCTION__, __LINE__);
	securesoho_get_curversion(cur_version);

	if(NULL==version || strcmp(cur_version, version)){
		/* do post_process when F/W updated */
		fwup_post_process("/conf_src/config_policy.xml");
	}
	return;
}

static int _print_usage(char *cmd)
{
	printf("Copyright (C) 2007, Alphanetworks, inc.\n");
	printf("Usage: %s [options]\n", cmd);
	printf("\t\t -e,--eraseflash                :erase the flash before writing\n");
	printf("\t\t -r,--reset                     :reset the config partion\n");
	printf("\t\t -s,--save                      :save the config to flash\n");
	printf("\t\t -l,--load                      :load the flash config\n");
	printf("\t\t -m,--monitor <time>            :start the flash monitor, the attribue \n");
	printf("\t\t -c,--change [Config File] <VAL=KEY>      :save the value and key\n");
	printf("\t\t -t,--savedir <srdir=targefile> :save the dir to the target file\n");
	printf("\t\t -x,--extract <srcfile=targetdir> :extact the file to target directory\n");
	printf("\t\t --mac <MAC>                    :To set MAC for mass-production\n");
	printf("\t\t --regiondata <clean>           :clean all region data sections to zero\n");
	printf("\t\t --regiondata <get>             :print region data info\n");
	printf("\t\t --regiondata <set tv_mode-region-location> :set region data info\n");
	printf("\t\t                                e.g.: 0-0-15, USA; 0-0-4, Canada.\n");
	return 0;
}

static int config_tool_cmd_fd=0;
static unsigned char ep_fifo_create_cb(void *readfd, void *writefd)
{
	config_tool_cmd_fd = *(int *)readfd;
	printf("F:%s:%d, readfd:%d\n", __FUNCTION__, __LINE__, config_tool_cmd_fd);
	return 0;
}

int main(int argc, char **argv)
{
	char product[32] = {0};
	char version[32] = {0};
	static config_post_process_handle_t *hd=NULL;
	int i, ret = 0;
	struct stat st;
	int op_flag=0;
	int monitor_per_sec=0;

#ifdef __DISABLE_DEBUG_OUTPUT__
	freopen("/dev/null", "a", stdout);
	freopen("/dev/null", "a", stderr);
#endif

	if(argc <= 1){
		_print_usage(argv[0]);
		goto out;
	}
	for(i=1;i<argc;i++){
		if(!strcmp(argv[i], "-e") || !strcmp(argv[i], "--eraseflash")){
			/* load the config */
			op_flag |= CONFIG_TOOL_OP_ERASE_FLASH;
		}
		if(!strcmp(argv[i], "-l") || !strcmp(argv[i], "--load")){
			/* load the config */
			op_flag |= CONFIG_TOOL_OP_LOAD;
		}
		if(!strcmp(argv[i], "-s") || !strcmp(argv[i], "--save") || !strcmp(argv[i], "-r") || !strcmp(argv[i], "--reset")){
			if(!strcmp(argv[i], "-r") || !strcmp(argv[i], "--reset"))
				securesoho_factory_default();
			/* save the config */
			op_flag |= CONFIG_TOOL_OP_SAVE;
			/* erase the flash */
			if(op_flag&CONFIG_TOOL_OP_ERASE_FLASH) {
				flash_op_t *p_flash_op = NULL;
				p_flash_op = create_flash_op();	
				flash_op_erase(p_flash_op, TARGET_FILE, NULL);
#ifdef CONF_CONFIG_MTD_PARTITION_BACKUP
				flash_op_erase(p_flash_op, TARGET_FILE2, NULL);
#endif
				destroy_flash_op(&p_flash_op);
			}
			/* save config */
			securesoho_get_curversion(version);
			mkconfig_translate_config_dir_to_file(SOURCE_DIR, TARGET_FILE, CONF_PRODUCT, version);
#ifdef CONF_CONFIG_MTD_PARTITION_BACKUP
                        mkconfig_translate_config_dir_to_file(SOURCE_DIR, TARGET_FILE2, CONF_PRODUCT, version);
#endif
		}
		if(!strcmp(argv[i], "-m") || !strcmp(argv[i], "--monitor")){
			int err=0;
			/* monitor the config */
			op_flag |= CONFIG_TOOL_OP_MONITOR;
			i++;
			if(i<argc){
				monitor_per_sec = atoi(argv[i]);
				if(monitor_per_sec == 0){
					err = 1;
				}
			}else{
				err = 2;
			}
			if(err){
				printf("ERR:-m %d\n", err);
				_print_usage(argv[0]);
				exit(0);
			}
		}
		if(!strcmp(argv[i], "-c") || !strcmp(argv[i], "--change")){
			int err=1;
			char *conf=NULL;
			char *str=NULL, *p;
			i++;
			while(i<argc){
				str = strdup(argv[i]);
				p = strchr(str, '=');
				if(NULL==p){
					if (conf) free(conf);
					conf = strdup(str);
					i++;
				}else{
					*p = '\0';
					if (NULL == conf){
						printf("config saved val:key =>(%s,%s)\n", str, p+1);
						securesoho_string_set(str,  p+1);
					}else{
						printf("config saved val:key [%s]=>(%s,%s)\n", conf, str, p+1);
						bs_config_string_set(conf, str, p+1);
					}
					err=0;
					break;
				}
				free(str);
			}
			if(err){
				printf("ERR:-c %d\n", err);
				_print_usage(argv[0]);
			}
			exit(0);
		}
		if(!strcmp(argv[i], "-t") || !strcmp(argv[i], "--savedir")){
			int err=0;
			char *str, *p;

			op_flag |= CONFIG_TOOL_OP_SAVEDIR;
			i++;
			if(i<argc){
				str = strdup(argv[i]);
				p = strchr(str, '=');
				if(p){
					*p = '\0';
 					/* erase the flash */
					if(op_flag&CONFIG_TOOL_OP_ERASE_FLASH) {
						flash_op_t *p_flash_op = NULL;
						p_flash_op = create_flash_op();	
						flash_op_erase(p_flash_op, p+1, NULL);
						destroy_flash_op(&p_flash_op);
					}
					printf("saved dir: (src,targe)=> (%s,%s)\n", str, p+1);
					securesoho_get_curversion(version);
					mkconfig_translate_config_dir_to_file(str, p+1, CONF_PRODUCT, version);
				}else err=1;
				free(str);
			}else err=2;
			if(err){
				printf("ERR:-c %d\n", err);
				_print_usage(argv[0]);
				exit(0);
			}
		}
		if(!strcmp(argv[i], "-x") || !strcmp(argv[i], "--extract")){
			int err=0;
			char *str;

			op_flag |= CONFIG_TOOL_OP_EXTRACT;
			i++;
			if(i<argc){
				str = strdup(argv[i]);
				mkconfig_translate_file_to_config_dir(str, product, version);
			}else err=2;
			if(err){
				printf("ERR:-c %d\n", err);
				_print_usage(argv[0]);
				exit(0);
			}
		}
		if(!strcmp(argv[i], "--mac")){
			i++;
			if (i<argc){
				bs_config_string_set(CONF_DEVICE_MP_CONFIG, "CONF_DEVICE_MP_MAC", argv[i]);
				mkconfig_translate_config_dir_to_file(MP_SOURCE_DIR, CONF_MP_MTD_PARTITION, CONF_PRODUCT, version);
				return ret == 0 ? -1 : 0; 	/* -1 fail */
			}else{
				_print_usage(argv[0]);
				return -1;
			}
		}
		if(!strcmp(argv[i], "--regiondata")){
			int err=0;
			char *str, *p1, *p2;

			op_flag |= CONFIG_TOOL_OP_REGION;
			i++;
			if(i<argc){
				device_region_data data;

				securesoho_get_curversion(version);
				
				do_mkdir(MP_SOURCE_DIR);
				mkconfig_translate_file_to_config_dir(CONF_MP_MTD_PARTITION, product, version);
				
				memset(&data, 0, sizeof(device_region_data));
				if(!strcmp(argv[i], "get")) {
					ret = securesoho_get_device_region_data(&data);
					return ret == 0 ? 0 : -1;	/* -1 fail. */
				}
				if(!strcmp(argv[i], "clean")) {
					ret = securesoho_set_device_region_data(&data);
					mkconfig_translate_config_dir_to_file(MP_SOURCE_DIR, CONF_MP_MTD_PARTITION, CONF_PRODUCT, version);
					return ret == 0 ? 0 : -1; 	/* -1 fail */
				}
				if(!strcmp(argv[i], "set")) {
					i++;
					str = strdup(argv[i]);

					p1 = strchr(str, '-');
					if(!p1) {
						err=1;
						free(str);
						printf("ERR:-c %d\n", err);
						_print_usage(argv[0]);
						return -1;
					}
					p2 = strchr(p1+1, '-');
					if(p2){
						char buf[8]={0};
						strncpy(buf, str, p1-str);
						data.version = 0;
						data.tv_mode = (unsigned char) atoi(buf);
						strncpy(buf, p1+1, p2-(p1+1));
						data.region = (unsigned char) atoi(buf);
						data.location = (unsigned char) atoi(p2+1);
						securesoho_set_device_region_data(&data);
						ret = mkconfig_translate_config_dir_to_file(MP_SOURCE_DIR, CONF_MP_MTD_PARTITION, CONF_PRODUCT, version);
						return ret == 0 ? 0 : -1; 	/* -1 fail */
					}else err=1;
					free(str);
				}
			}else err=2;
			if(err){
				printf("ERR:-c %d\n", err);
				_print_usage(argv[0]);
				return -1;
			}
		}
	}
	if(!op_flag){
		_print_usage(argv[0]);
	}
	if(op_flag & CONFIG_TOOL_OP_LOAD){
		hd = get_oem_handle(CONF_PRODUCT);
		
		/* mkdir /tmp/conf directyr */
		do_mkdir(SOURCE_DIR);
		
		/* load config */
		ret = mkconfig_translate_file_to_config_dir(TARGET_FILE, product, version);
#ifdef CONF_CONFIG_MTD_PARTITION_BACKUP
		if( ret < 0){
			ret = mkconfig_translate_file_to_config_dir(TARGET_FILE2, product, version);
		}
#endif
                
		if( ret < 0){
			securesoho_factory_default();
                }else {
			if(stat(SECURESOHO_CONFIG, &st) < 0) {
				fprintf(stderr, "can not find %s, we will securesoho_factory_default().\n", SECURESOHO_CONFIG);
				securesoho_factory_default();
			}
		}
		
		config_post_process(version);
		
		if(hd)
			hd->post_process(version);
	}
	if(op_flag & CONFIG_TOOL_OP_MONITOR){
		ptr_event_object_handler evt=NULL;
		int stop_flag=0;

		evt = create_event_proxy(EVENT_PROXY_FIFO, EPCONFIGTOOL_RD_FIFO, EPCONFIGTOOL_WR_FIFO, ep_fifo_create_cb);
		if(evt==NULL){
			/* fail to create the event proxy */
			exit(0);
		}
		flash_monitor_init();
		while(1){
			int fd, elen;
			struct timeval tv;
			CONFIG_TOOL_CMD *cmd;
			fd_set rfds;

			FD_ZERO(&rfds);
			FD_SET(config_tool_cmd_fd, &rfds);

			tv.tv_sec = monitor_per_sec;
			tv.tv_usec = 0;

			fd = select(config_tool_cmd_fd+1, &rfds, NULL, NULL, &tv);
			if(fd > 0 && FD_ISSET(config_tool_cmd_fd, &rfds)){
				/* receive t the command */
				cmd = NULL;
				event_proxy_get_event(evt, (void **)&cmd, &elen);
				if(cmd){
					switch(*cmd){
					case CONFIG_TOOL_CMD_STOP:
						fprintf(stderr, "F:%s:%d, CONFIG_TOOL_CMD_STOP\n", __FUNCTION__, __LINE__);
						stop_flag = 1;
						break;
					case CONFIG_TOOL_CMD_START:
						fprintf(stderr, "F:%s:%d, CONFIG_TOOL_CMD_START\n", __FUNCTION__, __LINE__);
						stop_flag = 0;
						break;
					case CONFIG_TOOL_CMD_DEAD:
						fprintf(stderr, "F:%s:%d, CONFIG_TOOL_CMD_DEAD\n", __FUNCTION__, __LINE__);
						goto dead;
						break;
					default:
						fprintf(stderr, "F:%s:%d, CONFIG_TOOL_CMD_UNKNOWN\n", __FUNCTION__, __LINE__);
						break;
					}
					free(cmd);
				}
			}
			if(!stop_flag){
				flash_monitor_checkforwrite(0);
			}
		}
	dead:
		flash_monitor_destroy();
	}
out:
	exit(0);
	
}
