/* Copyright (C) 2007, Alphanetworks, inc.
 * Author:  
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "securesoho.h"

void str_dec(char *s)
{
	int v = atoi(s);
	v--;
	sprintf(s,"%d",v);
}

void str_inc(char *s)
{
	int v = atoi(s);
	v++;
	sprintf(s,"%d",v);
}

void udn16_maker(const char *token, char *udn_container)
{
	int i, len;
	unsigned int x=0;
	unsigned short SHORT_MASK=0xFFFF;

	if(token){
		len = strlen(token);
		for(i=0; i<len; i++){
			srand((unsigned int)token[i]+x);
			x = rand();
		}
	}
	srand(x);
	sprintf(udn_container, "%04x%04x%04x%04x",
		rand()%SHORT_MASK, rand()%SHORT_MASK, rand()%SHORT_MASK, rand()%SHORT_MASK);
}

void udn_maker(const char *token, char *udn_container)
{
	unsigned char macaddr[6];
	int i, len;
	unsigned int x=0;
	unsigned short SHORT_MASK=0xFFFF;

	if(token){
		len = strlen(token);
		for(i=0; i<len; i++){
			srand((unsigned int)token[i]+x);
			x = rand();
		}
	}
	if(securesoho_nic_get_ifc_hwaddr(LAN_PORT, strlen(LAN_PORT), (char *)macaddr) < 0 )
	{
		return;
	}
	for(i=0; i<6; i++){
		srand((unsigned int)macaddr[i]+x);
		x = rand();
	}
	srand(x);
	sprintf(udn_container, "%04x%04x-%02x%02x-%02x%02x-%02x%02x-%04x%04x%04x",
			rand()%SHORT_MASK, rand()%SHORT_MASK,
			macaddr[0], macaddr[1], macaddr[2], 
			macaddr[3], macaddr[4], macaddr[5],
			rand()%SHORT_MASK, rand()%SHORT_MASK, rand()%SHORT_MASK);
}
