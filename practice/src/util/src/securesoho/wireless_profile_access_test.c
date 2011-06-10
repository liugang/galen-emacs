/*
 * =====================================================================================
 *
 *       Filename:  wireless_profile_access_test.c
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  11/11/2009 07:47:26 PM
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Joshua Lee Joshua_Lee@Alphanetworks.com
 *        Company:  Alpha Networks(Chengdu) Co., LTD Shanghai Branch
 *
 * =====================================================================================
 * vim:cindent:ts=8:sw=8:
 */
#include <stdio.h>

#include "securesoho.h"
#include "securesoho_wireless.h"
#include "wireless_profile_access.h"

static void wpdump(wp_node *wplist)
{
	wp_node *head, *node;
	struct list_head  *pos, *n;

	printf("enter %s\n\n", __func__);

	if (!wplist) {
		return ;
	}

	head = wplist;
	list_for_each_safe(pos, n, &head->list){
		node = list_entry(pos, wp_node, list);

		printf("w.ssid:%s \n", node->w.ssid);
		printf("w.bssid:%s \n", node->w.bssid);
		printf("w.channel:%d \n", node->w.channel);
		printf("w.wirelss_mode:%d \n", node->w.wireless_mode);
		printf("w.wirelss_type:%d \n", node->w.wireless_type);
		printf("w.encrypt_type:%d \n", node->w.encrypt_type);
		printf("w.auth_mode:%d \n", node->w.auth_mode);
		printf("w.wps_mode:%d \n", node->w.wps_mode);
		printf("w.wps_pin:%s \n", node->w.wps_pin);
		printf("w.wep_key_index:%d \n", node->w.wep_key_index);
		printf("w.wep_encrypt_level:%d \n", node->w.wep_encrypt_level);
		printf("w.wep_hexkey0:%s \n", node->w.wep_hexkey[0]);
		printf("w.wep_hexkey1:%s \n", node->w.wep_hexkey[1]);
		printf("w.wep_hexkey2:%s \n", node->w.wep_hexkey[2]);
		printf("w.wep_hexkey3:%s \n", node->w.wep_hexkey[3]);
		printf("w.wep_asciikey0:%s \n", node->w.wep_asciikey[0]);
		printf("w.wep_asciikey1:%s \n", node->w.wep_asciikey[1]);
		printf("w.wep_asciikey2:%s \n", node->w.wep_asciikey[2]);
		printf("w.wep_asciikey3:%s \n", node->w.wep_asciikey[3]);
		printf("w.wpa_psk:%s \n\n", node->w.wpa_psk);
	}

}

int test_count(wp_node *head){
	printf("The count is %d\n", wireless_profile_getcount(head));
	return 0;
}
int test_maxcount(wp_node *head){
	printf("The max count is %d\n", wireless_profile_get_maxcount());
	return 0;
}

int test_get_nth_node(wp_node *head){
	wp_node *node;
	wireless_profile_get_nth_node(head, 0, &node);
	if (node)
		printf("The 0 node ssid is %s\n", node->w.ssid);
	wireless_profile_get_nth_node(head, 1, &node);
	if (node)
		printf("The 0 node ssid is %s\n", node->w.ssid);
	return 0;
}

int test_contain(wp_node *head){
	wp_node *node;
	SecureSOHO_Wireless w;

	wireless_profile_get_nth_node(head, 0, &node);
	if (node)
		printf("The 0 node ssid is %s\n", node->w.ssid);
	mempcpy(&w, &node->w, sizeof(SecureSOHO_Wireless));
	printf("The list %s the ssid %s\n", wireless_profile_contains_of(head, &w) == 1? "contains" : "does not contaion", w.ssid);
	memcpy(w.ssid, "ssid", 4);
	printf("The list %s the ssid %s\n", wireless_profile_contains_of(head, &w) == 1? "contains" : "does not contaion", w.ssid);
	return 0;
}

int test_add(wp_node *head){
	wp_node *node, *new;
	SecureSOHO_Wireless w;

	wireless_profile_get_nth_node(head, 0, &node);
	if (!node){
		printf("Failed to get  node 0\n");
		return -1;
	}
	memset(&w, 0, sizeof(SecureSOHO_Wireless));
	memcpy(&w, &node->w, sizeof(SecureSOHO_Wireless));
	wpdump(head);

	new = calloc(sizeof(wp_node), 1);
	memcpy(&new->w, &w, sizeof(SecureSOHO_Wireless));
	strcpy(new->w.ssid, "test1");
	strcpy(new->w.bssid, "00:11:22:33:44:55");
	wireless_profile_add(head, new);
	wpdump(head);

	new = calloc(sizeof(wp_node), 1);
	memcpy(&new->w, &w, sizeof(SecureSOHO_Wireless));
	strcpy(new->w.ssid, "test2");
	strcpy(new->w.bssid, "00:11:22:33:44:56");
	wireless_profile_add(head, new);
	wpdump(head);


	new = calloc(sizeof(wp_node), 1);
	memcpy(&new->w, &w, sizeof(SecureSOHO_Wireless));
	strcpy(new->w.ssid, "test3");
	strcpy(new->w.bssid, "00:11:22:33:44:57");
	wireless_profile_add(head, new);
	wpdump(head);

	new = calloc(sizeof(wp_node), 1);
	memcpy(&new->w, &w, sizeof(SecureSOHO_Wireless));
	strcpy(new->w.ssid, "newtest1");
	strcpy(new->w.bssid, "00:11:22:33:44:55");
	wireless_profile_add(head, new);
	wpdump(head);

	new = calloc(sizeof(wp_node), 1);
	memcpy(&new->w, &w, sizeof(SecureSOHO_Wireless));
	strcpy(new->w.ssid, "test2");
	strcpy(new->w.bssid, "00:11:22:33:44:56");
	wireless_profile_add(head, new);
	wpdump(head);

	new = calloc(sizeof(wp_node), 1);
	memcpy(&new->w, &w, sizeof(SecureSOHO_Wireless));
	strcpy(new->w.ssid, "test2");
	strcpy(new->w.bssid, "00:11:22:33:44:57");
	wireless_profile_add(head, new);
	wpdump(head);
	return 0;
}

int main(){
	wp_node wplist;
	int count;


	wireless_profile_init(&wplist, &count);
	test_count(&wplist);
	test_maxcount(&wplist);
	test_get_nth_node(&wplist);
	test_contain(&wplist);
	test_add(&wplist);
	wireless_profile_destroy(&wplist);
	return 0;
}

