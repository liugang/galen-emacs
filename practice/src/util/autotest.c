/* Copyright (C) 2005, Alphanetworks, inc.
 * Author: wills_yin@alphanetworks.com
 * $Header: /data/cvsroot/DMA/util/autotest.c,v 1.1.2.10 2006-12-07 06:31:16 ken Exp $
 * vim:cindent:ts=8:sw=8:
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h> 
#include <net/if.h> 
#include <arpa/inet.h>
#include <ctype.h>
#include <unistd.h>

#include "securesoho.h"

typedef struct comp_t		comp_t;
typedef struct func_t		func_t;
typedef int (* disp_func )(int cmd);

struct func_t {
	int			index;
	const char		*usage;
};

struct comp_t {
	int		index;
	const char	*name;
	disp_func	cmd_func;
	struct func_t	*fun_list;
};

static int network_cmd_func( int index );
static int network_wl_cmd_func( int index );
static int network_sl_cmd_func( int index );
static int config_access_cmd_func( int index );

/*
 * For automatic test. the comps and function list can be read from some 
 * input (xml)files.
 */
func_t network_func_list[] = 
{
	{0, "GetIfHwAddr"},
	{1, "WaitInterfaceUp"},
	{2, "network_sync_restart"},
	{3, "get_max_bit_rate"},
	{4, "get_nic_type"},
	{-1, NULL}
};

func_t network_sl_func_list[] = 
{
	{0, "SetLanStatic"},
	{1, "SetLanDhcp"},
	{2, "GetLan"},
	{3, "GetState"},
	{4, "GetLinkStatus"},
	{-1, NULL}
};

func_t network_wl_func_list[] =
{
	{0, "AutoTestSubModule"},
	{1, "SetWirelessNo"},
	{2, "SetWirelessWep64"},
	{3, "SetWirelessWep128"},
	{4, "SetWirelessWPA"},
	{5, "GetLinkStatus"},
	{-1, NULL}
};

func_t config_access_func_list[] = 
{
	{0, "securesoho_string_set"},
	{1, "securesoho_string_get"},
	{-1, NULL}
};

static struct comp_t comps[] = 
{
	{
		0, "securesoho network", 
		network_cmd_func,
		network_func_list

	},
	{
		0, "securesoho wired network save/load", 
		network_sl_cmd_func,
		network_sl_func_list

	},
	{
		0, "securesoho wireless network save/load", 
		network_wl_cmd_func,
		network_wl_func_list

	},
	{
		0, "config access function test case", 
		config_access_cmd_func,
		config_access_func_list
	},
	{
		-1, NULL, NULL, NULL
	}
};

static int print_net_address(char *add)
{
	int i = 0;
	do {
		printf("%c",*add++);
	}while(i++ < 16 && *add != 0);
	return 0;

}

static int config_access_cmd_func(int index)
{
	switch(index) {
	case 0:
		securesoho_string_set("xxx", "001"); 
		break;
	case 1:
		break;
	default:
		break;
	}
	return 0;
}

static int network_wl_cmd_func(int index)
{
	int autotest=0;
	char ch;
	SecureSOHO_LAN	lan;
	SecureSOHO_Wireless wireless;
	NETWORK_STATE state;

	memset(&lan, '\0', sizeof(SecureSOHO_LAN));
	lan.lan_type = LAN_DHCP;
	lan.con_type = CON_WIRELESS;
	securesoho_lan_set(&lan, 0);
	securesoho_wireless_get(&wireless);
	switch(index){
	case 0:
		/* auto test the sub module */
		autotest = 1;
	case 1: 
		/* test NONE enctryption wireless connection */
		printf("*** please set the router no wireless encryption mode[test1], then press 'Enter' key \n");
		while ( (ch = fgetc(stdin)) != '\n'){
			/* do nothing */
		}
		sprintf(wireless.ssid, "test1");
		wireless.wireless_type = WIRELESS_INFRASTRUCTURE;  /* Insfracutre */
		wireless.encrypt_type= WIRELESS_ENCRYPT_NONE; /* Disable Security */
		securesoho_wireless_set(&wireless, 0);
		printf("\033[1;45m ===============  %s(), %d... ================== \033[0m  \n", __FUNCTION__, __LINE__);
		network_restart2(0);
		state = network_get_state();
		while(state != NETWORK_STATE_DONE){
			sleep(2);
			state = network_get_state();
			printf("case 1[no encryption]: state:%d\n", state);
		}
		printf("*** No wireless encryption is ok \n");
		if(!autotest) break;
	case 2:
		/* test WEP 64 */
		printf("*** please set the router 64WEP mode->[test2@1122334455], then press 'Enter' key \n");
		while ( (ch = fgetc(stdin)) != '\n'){
			/* do nothing */
		}
		sprintf(wireless.ssid, "test2");
		wireless.wireless_type = WIRELESS_INFRASTRUCTURE;  /* Insfracutre */
		wireless.encrypt_type = WIRELESS_ENCRYPT_WEP;  /* WEP security */
		wireless.wep_encrypt_level= 1; /* WEP 64bit Hex key */
		sprintf(wireless.wep_hexkey[0], "1122334455");
		securesoho_wireless_set(&wireless, 0);
		printf("\033[1;45m ===============  %s(), %d... ================== \033[0m  \n", __FUNCTION__, __LINE__);
		network_restart2(0);
		state = network_get_state();
		while(state != NETWORK_STATE_DONE){
			sleep(2);
			state = network_get_state();
			printf("case 2[WEP 64bits]: state:%d\n", state);
		}
		printf("*** wireless WEP 64bits is ok \n");
		if(!autotest) break;
	case 3:
		/* test WEP 128 */
		printf("*** please set the router 128WEP mode->[test3@11223344551122334455112233], then press 'Enter' key \n");
		while ( (ch = fgetc(stdin)) != '\n'){
			/* do nothing */
		}
		sprintf(wireless.ssid, "test3");
		wireless.wireless_type = WIRELESS_INFRASTRUCTURE;  /* Insfracutre */
		wireless.encrypt_type= WIRELESS_ENCRYPT_WEP;  /*  WEP security */
		wireless.wep_encrypt_level = 2; /* WEP 128bit Hex key */
		sprintf(wireless.wep_hexkey[0], "11223344551122334455112233");
		securesoho_wireless_set(&wireless, 0);
		printf("\033[1;45m ===============  %s(), %d... ================== \033[0m  \n", __FUNCTION__, __LINE__);
		network_restart2(0);
		state = network_get_state();
		while(state != NETWORK_STATE_DONE){
			sleep(2);
			state = network_get_state();
			printf("case 3[WEP 128bits]: state:%d\n", state);
		}
		printf("*** wireless WEP 128bits is ok \n");
		if(!autotest) break;
	case 4:
		/* test WPA */
		printf("*** please set the router WPA mode[test4@123456789], then press 'Enter' key \n");
		while ( (ch = fgetc(stdin)) != '\n'){
			/* do nothing */
		}
		sprintf(wireless.ssid, "test4");
		wireless.wireless_type = WIRELESS_INFRASTRUCTURE;  /* Insfracutre */
		wireless.auth_mode = WIRELESS_AUTH_WPAPSK;  /* WPA-PSK security method */
		wireless.encrypt_type = WIRELESS_ENCRYPT_TKIP;	/* TKIP encryption */
		sprintf(wireless.wpa_psk, "123456789");
		securesoho_wireless_set(&wireless, 0);
		printf("\033[1;45m ===============  %s(), %d... ================== \033[0m  \n", __FUNCTION__, __LINE__);
		network_restart2(0);
		state = network_get_state();
		while(state != NETWORK_STATE_DONE){
			sleep(2);
			state = network_get_state();
			printf("case 4[WPA-PSK]: state:%d\n", state);
		}
		printf("*** wireless WPA-PSK is ok \n");
		if(!autotest) break;
	case 5:
		/* Get Link Status */
		printf("%s network link status[%d](1:up, 0:down)\n", "eth1", securesoho_nic_check_ifc_link("eth1", 4));
		break;
	}
	return 0;
}
static int network_sl_cmd_func( int index )
{
	SecureSOHO_LAN	 lan;
	int i = 0;
	NETWORK_STATE state;

	switch(index){
	case 0: // set static
		lan.con_type = CON_WIRED;
		lan.lan_type = LAN_FIXEDIP;
		
		strncpy(lan.ip, "172.18.82.169", sizeof(lan.ip));

		strncpy(lan.netmask, "255.255.254.0", sizeof(lan.netmask));
		strncpy(lan.gateway, "172.18.83.254", sizeof(lan.gateway));
		strncpy(lan.dns, "172.18.83.254", sizeof(lan.dns));

		lan.set_dns_srv = MANUAL;
		securesoho_lan_set(&lan, 1);
		break;
	case 1: // set dhcp
		lan.con_type = CON_WIRED;
		lan.lan_type = LAN_DHCP;

		strncpy(lan.ip, "172.18.82.169", sizeof(lan.ip));
		
		strncpy(lan.netmask, "255.255.254.0", sizeof(lan.netmask));
		strncpy(lan.gateway, "172.18.83.254", sizeof(lan.gateway));
		strncpy(lan.dns, "172.18.83.254", sizeof(lan.dns));

		lan.set_dns_srv = AUTO;
		securesoho_lan_set(&lan, 1);
		printf("getting ip ... ");
		if (i == 20)
			printf("\n fail to get dhcp in 10 seconds\n");
		else
			printf("\n dhcp succeed, use get to see result\n");
		break;
	case 2: // load lan
		securesoho_lan_get(&lan);
		printf("current lan settings:\n");
		printf("ip: "); print_net_address(lan.ip); printf("\n");
		printf("netmask: "); print_net_address(lan.netmask); printf("\n");
		printf("gateway: "); print_net_address(lan.gateway); printf("\n");
		printf("dns: "); print_net_address(lan.dns); printf("\n");
		break;
	case 3:
		state = network_get_state();
		printf("\n");
		switch(state){
		case NETWORK_STATE_INIT:
			printf("state: NETWORK_STATE_INIT\n");
			break;
		case NETWORK_STATE_WIRELESS_CONNECTING:
			printf("state: NETWORK_STATE_WIRELESS_CONNECTING\n");
			break;
		case NETWORK_STATE_WIRELESS_TIMEOUT:
			printf("state: NETWORK_STATE_WIRELESS_TIMEOUT\n");
			break;
		case NETWORK_STATE_REQUESTING_IP:
			printf("state: NETWORK_STATE_REQUESTING_IP\n");
			break;
		case NETWORK_STATE_DONE:
			printf("state: NETWORK_STATE_DONE\n");
			break;
		case NETWORK_STATE_UNKNOWN:
			printf("state: NETWORK_STATE_UNKNOWN\n");
			break;
		default:
			printf("state: NETWORK_STATE_WHY_HERE\n");
			break;
		}
	case 4:
		/* Get Link Status */
		printf("%s network link status[%d](1:up, 0:down)\n", "eth0", securesoho_nic_check_ifc_link("eth0", 4));
	default:
		break;
	}
	return 0;
}

static int network_cmd_func( int index )
{
	char hwaddr[16] = {0};

	switch(index){
	case 0:
		if ( securesoho_nic_get_ifc_hwaddr("eth0", 4 ,hwaddr) < 0 )
			printf("Failed to get etho address\n");
		else
			printf("Success! The eth0's :%02x:%02x:%02x:%02x:%02x:%02x\n",
				hwaddr[0], hwaddr[1], hwaddr[2], 
				hwaddr[3], hwaddr[4], hwaddr[5]);
		break;
	case 1:
		if ( securesoho_nic_wait_ifc_up("eth0", 5) > 0 )
			printf("The interface eth0 is up!\n");
		else
			printf("The interface eth0 is down or dosen't exist!\n");
	case 2:
		network_sync_restart();
		break;
	case 3:
		securesoho_string_set("virdev_int", "eth0");
		printf("securesoho get eth0 max bit rate:%d\n", securesoho_nic_max_bit_rate());
		securesoho_string_set("virdev_int", "eth1");
		printf("securesoho get eth1 max bit rate:%d\n", securesoho_nic_max_bit_rate());
		break;
	case 4:
		securesoho_string_set("virdev_int", "eth0");
		printf("securesoho get eth0 nic type:%d\n", securesoho_nic_get_type());
		securesoho_string_set("virdev_int", "eth1");
		printf("securesoho get eth1 nic type:%d\n", securesoho_nic_get_type());
		break;
	default:
		printf("Please select a right function index in the component!\n");
	}

	return 0;
}

static void show_usage( comp_t* cp)
{
	int i;
	func_t *f;

	if ( !cp)
		return;

	printf("Component %s:\n", cp->name);
	printf("The available APIs are:\n");
	
	i = 0;
	for (f = cp->fun_list; f && f->index != -1; f++ ) {
		printf("\tFunc %d, %-25s\n", f->index, f->usage);
		i++;
	}
	printf("\n");

}

int main( int argc, char* argv[] )
{
	int i, total;
	int ch, comp_inx, func_inx;

	if(argc != 2) {
		printf("usage: %s <host> \n", argv[1]);
		return 0;
	}

#if 0
	securesoho_network_init_host(argv[1]);
#endif

	total = sizeof(comps) / sizeof(struct comp_t) - 1;
	do {
		comp_inx = 0;

		printf("Please select a component to test:\n");
		for ( i = 0; i < total; i++ ) 
			printf("\tComponent %d, %s\n", i, comps[i].name);

		while ( (ch = fgetc(stdin)) != '\n')
			comp_inx = comp_inx *10 + (ch - '0');

		if (comp_inx >= total) {
			printf("The input is too big\n");
			continue;
		}

		printf("Please select a function to test:\n");
		show_usage( &comps[comp_inx] ); 

		func_inx = 0;
		while ( (ch = fgetc(stdin)) != '\n')
			func_inx = func_inx * 10 + (ch - '0');
		if (comps[comp_inx].cmd_func)
			comps[comp_inx].cmd_func(func_inx);

	} while(1);


	return 0;
}

