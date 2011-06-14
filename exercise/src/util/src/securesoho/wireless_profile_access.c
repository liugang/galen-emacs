/* 
 * Copyright (C) 2006, ALPHA Networks, inc.
 * Author:  Tao Yu<tao_yu@alphanetworks.com>
 * $Header: /data/cvsroot/DMA/util/src/securesoho/wireless_profile_access.c,v 1.1.2.12 2007-07-31 03:27:36 qi_lu Exp $
 * vim:cindent:ts=8:
 */
#include <mxml.h>
#include "wireless_profile_access.h"
#include "securesoho.h"

#define  MYLOG_CATEGORGE_NAME "util"
#include "mylog.h"

#define	 MXML_HEAD_STRING		"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

#define WIRELESS_PROFILE_XML		"/conf/wireless_profile.xml"
#define TAG_ATTR_VALUE			"value"
#define TAG_PROFILE_MAX_NUM		"profiles_max_num"
#define DEFAULT_PROFILE_MAX_NUM		(6)
#define TAG_PROFILE			"profile"
#define TAG_WIRELESS_MODE		"WIRELESS_MODE"
#define TAG_WIRELESS_SSID		"WIRELESS_SSID"
#define TAG_WIRELESS_BSSID		"WIRELESS_BSSID"
#define TAG_WIRELESS_CHANNEL		"WIRELESS_CHANNEL"
#define TAG_WIRELESS_TYPE		"WIRELESS_TYPE"
#define TAG_WIRELESS_ENCRYPT_TYPE	"WIRELESS_ENCRYPT_TYPE"
#define TAG_WIRELESS_WPS_MODE		"WIRELESS_WPS_MODE"
#define TAG_WIRELESS_WPS_PIN		"WIRELESS_WPS_PIN"
#define TAG_WIRELESS_AUTH_MODE		"WIRELESS_AUTH_MODE"
#define TAG_WIRELESS_WEP_ENCRYPT_LEVEL	"WIRELESS_WEP_ENCRYPT_LEVEL"
#define TAG_WIRELESS_WEP_KEY_INDEX	"WIRELESS_WEP_KEY_INDEX"
#define TAG_WIRELESS_WEP_HEXKEY0	"WIRELESS_WEP_HEXKEY0"
#define TAG_WIRELESS_WEP_HEXKEY1	"WIRELESS_WEP_HEXKEY1"
#define TAG_WIRELESS_WEP_HEXKEY2	"WIRELESS_WEP_HEXKEY2"
#define TAG_WIRELESS_WEP_HEXKEY3	"WIRELESS_WEP_HEXKEY3"
#define TAG_WIRELESS_WEP_ASCIIKEY0	"WIRELESS_WEP_ASCIIKEY0"
#define TAG_WIRELESS_WEP_ASCIIKEY1	"WIRELESS_WEP_ASCIIKEY1"
#define TAG_WIRELESS_WEP_ASCIIKEY2	"WIRELESS_WEP_ASCIIKEY2"
#define TAG_WIRELESS_WEP_ASCIIKEY3	"WIRELESS_WEP_ASCIIKEY3"
#define TAG_WIRELESS_WPA_PSK		"WIRELESS_WPA_PSK"

static int profiles_max_num = DEFAULT_PROFILE_MAX_NUM;

static int wireless_profile_parse_profile_node(mxml_node_t *node, 
					       wp_node **wpnode);
#if 0
static int wireless_profile_getpos(wp_node *wplist, wp_node *node);
#endif

/* load from profile xml */
int wireless_profile_init(wp_node *wplist, int *count)
{
	FILE  *fp = NULL;
	mxml_node_t *root = NULL, *node = NULL, *curnode;
	wp_node *head = NULL;
	int retval = -1;

	mylog_trace("enter %s", __func__);

	if(!wplist || !count) {
                mylog_error("%s:%d Failed to load wireless profile\n", __FUNCTION__, __LINE__);
		return -1;
        }
	
	head = wplist;
	*count = 0;

	INIT_LIST_HEAD(&head->list);

	if ((fp = fopen(WIRELESS_PROFILE_XML, "r")) == NULL) {
		mylog_error("Failed to Open %s:%s\n", WIRELESS_PROFILE_XML, strerror(errno));
		goto end_init;
	}
	
	if ((root = mxmlLoadFile(MXML_NO_PARENT, fp, MXML_NO_CALLBACK)) == NULL) {
                mylog_error("%s:%d Failed to load wireless profile\n", __FUNCTION__, __LINE__);
		goto end_init;
        }
	
	node = mxmlFindElement(root, root, TAG_PROFILE, NULL, NULL, MXML_DESCEND);
	if (node == NULL) {
                mylog_error("%s:%d Failed to load wireless profile\n", __FUNCTION__, __LINE__);
		goto end_init;
        }

	for (curnode = node; curnode; curnode = curnode->next) {
		int ret = 0;
		wp_node *new = NULL;

		if (curnode->type != MXML_ELEMENT || 
		    strcmp(curnode->value.element.name, TAG_PROFILE)) 
			continue;

		ret = wireless_profile_parse_profile_node(curnode, &new);
		if (ret == 0 && new != NULL){
			wireless_profile_add_tail(head, new);
			(*count) ++;
			if (*count >= DEFAULT_PROFILE_MAX_NUM){
				mylog_error("%s:%d The profile is full(%d)", __FUNCTION__, __LINE__, *count);
				retval = 0;
				goto end_init;
			}
		}
	}
	retval = 0;

end_init:
	if (fp) 
		fclose(fp);
	if (root) 
		mxmlDelete(root);

	return retval;
}

int wireless_profile_destroy(wp_node *wplist)
{
	wp_node *head, *tmp;
	struct list_head  *pos, *n;
	
	mylog_trace("enter %s", __func__);

	if (!wplist) {
		mylog_error("%s:%d the wplist is null", __func__, __LINE__);
		return -1;
	}

	head = wplist;
	list_for_each_safe(pos, n, &head->list){
		tmp = list_entry(pos, wp_node, list);
		list_del(pos);
		free(tmp);
	}

	return 0;
}

static int is_same_ap(SecureSOHO_Wireless *w1, SecureSOHO_Wireless *w2)
{
	return !strcasecmp(w1->ssid, w2->ssid);
}

int wireless_profile_contains_of(wp_node *wplist, SecureSOHO_Wireless *w)
{
	wp_node *head, *tmp;
	struct list_head  *pos, *n;

	mylog_trace("enter %s", __func__);

	if (!wplist) {
		mylog_error("%s:%d the wplist is null", __func__, __LINE__);
		return -1;
	}

	head = wplist;
	list_for_each_safe(pos, n, &head->list){
		tmp = list_entry(pos, wp_node, list);
		if (is_same_ap(&tmp->w, w))
			return 1;
	}

	return 0;
}


#ifdef __WP_TEST__
/* used for test */
static void showwpnode(wp_node *wplist)
{
	wp_node *head, *node;
	struct list_head  *pos, *n;

	mylog_trace("enter %s", __func__);

	if (!wplist) {
		return ;
	}

	head = wplist;
	list_for_each_safe(pos, n, &head->list){
		node = list_entry(pos, wp_node, list);

		mylog_info("w.ssid:%s \n", node->w.ssid);
		mylog_info("w.bssid:%s \n", node->w.bssid);
		mylog_info("w.channel:%d \n", node->w.channel);
		mylog_info("w.wirelss_mode:%d \n", node->w.wireless_mode);
		mylog_info("w.wirelss_type:%d \n", node->w.wireless_type);
		mylog_info("w.encrypt_type:%d \n", node->w.encrypt_type);
		mylog_info("w.auth_mode:%d \n", node->w.auth_mode);
		mylog_info("w.wps_mode:%d \n", node->w.wps_mode);
		mylog_info("w.wps_pin:%s \n", node->w.wps_pin);
		mylog_info("w.wep_key_index:%d \n", node->w.wep_key_index);
		mylog_info("w.wep_encrypt_level:%d \n", node->w.wep_encrypt_level);
		mylog_info("w.wep_hexkey0:%s \n", node->w.wep_hexkey[0]);
		mylog_info("w.wep_hexkey1:%s \n", node->w.wep_hexkey[1]);
		mylog_info("w.wep_hexkey2:%s \n", node->w.wep_hexkey[2]);
		mylog_info("w.wep_hexkey3:%s \n", node->w.wep_hexkey[3]);
		mylog_info("w.wep_asciikey0:%s \n", node->w.wep_asciikey[0]);
		mylog_info("w.wep_asciikey1:%s \n", node->w.wep_asciikey[1]);
		mylog_info("w.wep_asciikey2:%s \n", node->w.wep_asciikey[2]);
		mylog_info("w.wep_asciikey3:%s \n", node->w.wep_asciikey[3]);
		mylog_info("w.wpa_psk:%s \n", node->wpa_psk);
	}

}
#endif

/*
 * Parse the xml profile node to get the ssid info
 */
static int wireless_profile_parse_profile_node(mxml_node_t *node, 
		wp_node **wpnode)
{
	mxml_node_t *curnode;

	if (!node){
		mylog_error("%s:%d node is NULL\n", __func__, __LINE__);
		return -1;
	}

	if ((*wpnode = malloc(sizeof(wp_node))) == NULL){
		mylog_error("%s:%d out of memory\n", __func__, __LINE__);
		return -1;
	}
	bzero(*wpnode, sizeof(wp_node)); 

	for (curnode = node->child; curnode; curnode = curnode->next) {
		if (curnode->type != MXML_ELEMENT)
			continue;

		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_MODE)) 
			(*wpnode)->w.wireless_mode = atoi(mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_SSID))
			strcpy((*wpnode)->w.ssid, mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_BSSID))
			strcpy((*wpnode)->w.bssid, mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_CHANNEL))
			(*wpnode)->w.channel = atoi(mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_TYPE))
			(*wpnode)->w.wireless_type = atoi(mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_ENCRYPT_TYPE))
			(*wpnode)->w.encrypt_type = atoi(mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_AUTH_MODE))
			(*wpnode)->w.auth_mode = atoi(mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WPS_MODE))
			(*wpnode)->w.wps_mode = atoi(mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WPS_PIN))
			strcpy((*wpnode)->w.wps_pin, mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WEP_KEY_INDEX))
			(*wpnode)->w.wep_key_index = atoi(mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WEP_ENCRYPT_LEVEL))
			(*wpnode)->w.wep_encrypt_level = atoi(mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WEP_HEXKEY0))
			strcpy((*wpnode)->w.wep_hexkey[0], mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WEP_HEXKEY1))
			strcpy((*wpnode)->w.wep_hexkey[1], mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WEP_HEXKEY2))
			strcpy((*wpnode)->w.wep_hexkey[2], mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WEP_HEXKEY3))
			strcpy((*wpnode)->w.wep_hexkey[3], mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WEP_ASCIIKEY0))
			strcpy((*wpnode)->w.wep_asciikey[0], mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WEP_ASCIIKEY1))
			strcpy((*wpnode)->w.wep_asciikey[1], mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WEP_ASCIIKEY2))
			strcpy((*wpnode)->w.wep_asciikey[2], mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WEP_ASCIIKEY3))
			strcpy((*wpnode)->w.wep_asciikey[3], mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
		if (0 == strcmp(curnode->value.element.name, TAG_WIRELESS_WPA_PSK))
			strcpy((*wpnode)->w.wpa_psk, mxmlElementGetAttr(curnode, TAG_ATTR_VALUE));
	}

	/*
	 * FIXME: 
	 *     Since there's no BSSID if user manually input the SSID, so do 
	 *     not invalidate the bssid here. We'll get the BSSID and fill in 
	 *     the configuration later if the device connected to the AP.
	 */
#if  0
	if (strlen((*wpnode)->w.bssid) != 17) {
		mylog_error("%s:%d %s is invalid bssid, ignore it", __func__, __LINE__, (*wpnode)->w.bssid);
		free(*wpnode);
		*wpnode = NULL;
		return -1;
	}
#endif
	return 0;
}

/* pos: 0 based */
int wireless_profile_get_nth_node(wp_node *wplist, int pos, wp_node **node)
{
	wp_node *head, *tmp;
	struct list_head  *entry, *n;
	int count = pos;

	mylog_trace("enter %s", __func__);

	if (!wplist) {
		return -1;
	}

	head = wplist;
	list_for_each_safe(entry, n, &head->list){
		if (count-- == 0){ 
			tmp = list_entry(entry, wp_node, list);
			*node = tmp;
			return 0;
		}	
	}

	*node = NULL;
	return -1;

}

int     wireless_profile_get_maxcount(void)
{
	return profiles_max_num;
}

int wireless_profile_getcount(wp_node *wplist)
{
	int count = 0;
	wp_node *head;
	struct list_head  *pos, *n;

	if (!wplist) return 0;

	head = wplist;
	list_for_each_safe(pos, n, &head->list)
		count ++;

	return count;
}

int wireless_profile_add(wp_node *wplist, wp_node *node)	
{
	wp_node *head, *tmp;
	struct list_head  *pos, *n;

	if (!wplist || !node) {
		mylog_error("%s:%d the list or node is NULL", __func__, __LINE__);
		return -1;
	}

	head = wplist;
	list_for_each_safe(pos, n, &head->list){
		tmp = list_entry(pos, wp_node, list);
		if (is_same_ap(&(tmp->w), &(node->w))) {
			mylog_error("The profile has already added to the list");
			mylog_error("Replace with the new one");
			list_replace(pos, &node->list);
			free(tmp);
			return 0;
		}
		
	}
	list_add(&node->list, &head->list);

	return 0;
}

int wireless_profile_replace(wp_node *wplist, wp_node *node, int index)
{
	wp_node *head, *tmp;
	struct list_head  *pos, *n;
	int count = 0;

	if(!wplist || !node){
		mylog_error("%s:%d the list or node is NULL", __func__, __LINE__);
		return -1;
	}

	head = wplist;
	list_for_each_safe(pos, n, &head->list){
		tmp = list_entry(pos, wp_node, list);
		if (count++ == index){
			list_replace(pos, &node->list);
			free(tmp);
			return 0;
		}
	}

	mylog_error("%s:%d the index %d is not in the list", __func__, __LINE__, index);
	return -1;

}

int wireless_profile_add_tail(wp_node *wplist, wp_node *node)	
{
	wp_node *head, *tmp;
	struct list_head  *pos, *n;

	if(!wplist || !node){
		mylog_error("%s:%d the list or node is NULL", __func__, __LINE__);
		return -1;
	}

	head = wplist;
	list_for_each_safe(pos, n, &head->list){
		tmp = list_entry(pos, wp_node, list);
		if (is_same_ap(&(tmp->w), &(node->w))) {
			mylog_error("The profile has already added to the list");
			mylog_error("Replace with the new one");
			list_replace(pos, &node->list);
			free(tmp);
			return 0;
		}
	}
	list_add_tail(&node->list, &head->list);

	return 0;
}


int wireless_profile_remove(wp_node *wplist, wp_node *node)
{
	if (!node){
		mylog_error("%s:%d the node is NULL", __func__, __LINE__);
		return -1;
	}
	list_del(&node->list);
	free(node);

	return 0;
}

#if 0
/* pos: 0 based 
 * found: return pos; 
 * not found, no node: return -1;
 */
static int wireless_profile_getpos(wp_node *wplist, wp_node *node)
{
	int count = 0;
	wp_node *head, *tmp;
	struct list_head  *pos, *n;

	if (!wplist) 
		return -1;

	head = wplist;
	list_for_each_safe(pos, n, &head->list){
		tmp = list_entry(pos, wp_node, list);
		if (node == tmp)
			return count;
		count ++;
	}

	return -1;
}
#endif

static	const char*  Indent[] = {NULL, "\t", "\t\t", "\t\t\t", "\t\t\t\t"};
/*
 * 'whitespace_cb()' - Let the mxmlSaveFile() function know when to insert
 *		       newlines and tabs...
 */
static const char *save_whitespace_cb(mxml_node_t* node,	int where)	
{
	int	     level, total;
	const char  *name;
	mxml_node_t *parent;

	if (!node || node->type !=MXML_ELEMENT)
		return NULL;

	name = node->value.element.name;

	for (level = -1, parent = node->parent; 
			parent; 
			level++, parent = parent->parent);
	total = sizeof(Indent)/sizeof(Indent[0]);
	if (level >= total)
		level = total - 1;

	if (name && strstr(name, "?xml")) {
		if ( where == MXML_WS_BEFORE_OPEN)
			return NULL;
		else if (where == MXML_WS_AFTER_OPEN) 
			return "\n";
		else 
			return NULL;
	}
	if (name) {
		if ( where == MXML_WS_BEFORE_OPEN) {
			return Indent[level];
		}
		else if (where == MXML_WS_AFTER_OPEN) {
			return "\n";
		}
		else if (where == MXML_WS_BEFORE_CLOSE){ 
			return Indent[level];
		}
		else if (where == MXML_WS_AFTER_CLOSE) {
			return "\n";
		}
	} else if (where == MXML_WS_BEFORE_OPEN) {
		return Indent[level];
	} else if (where == MXML_WS_AFTER_CLOSE) {
		return "\n";
	} 
	return NULL;
}

void wireless_profile_create_xml_wp_content(wp_node *wpnode, mxml_node_t *profile)
{
	char buf[256]={0};
	mxml_node_t *node;

	if(!wpnode || !profile) return;

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_SSID)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%s", wpnode->w.ssid);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_BSSID)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%s", wpnode->w.bssid);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_CHANNEL)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%d", wpnode->w.channel);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_TYPE)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%d", wpnode->w.wireless_type);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_MODE)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%d", wpnode->w.wireless_mode);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_ENCRYPT_TYPE)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%d", wpnode->w.encrypt_type);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_AUTH_MODE)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%d", wpnode->w.auth_mode);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WPS_MODE)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%d", wpnode->w.wps_mode);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WPS_PIN)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%s", wpnode->w.wps_pin);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WEP_KEY_INDEX)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%d", wpnode->w.wep_key_index);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WEP_ENCRYPT_LEVEL)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%d", wpnode->w.wep_encrypt_level);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WEP_ASCIIKEY0)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%s", wpnode->w.wep_asciikey[0]);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WEP_ASCIIKEY1)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%s", wpnode->w.wep_asciikey[1]);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WEP_ASCIIKEY2)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%s", wpnode->w.wep_asciikey[2]);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WEP_ASCIIKEY3)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%s", wpnode->w.wep_asciikey[3]);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WEP_HEXKEY0)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%s", wpnode->w.wep_hexkey[0]);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WEP_HEXKEY1)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%s", wpnode->w.wep_hexkey[1]);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WEP_HEXKEY2)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%s", wpnode->w.wep_hexkey[2]);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WEP_HEXKEY3)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%s", wpnode->w.wep_hexkey[3]);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);

	if ((node = mxmlNewElement(profile, TAG_WIRELESS_WPA_PSK)) == NULL) {
		goto end_create;
	}
	sprintf(buf, "%s", wpnode->w.wpa_psk);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);


	return;

end_create:
	if (profile) mxmlDelete(profile);
}

int wireless_profile_save(wp_node *wplist)
{
	mxml_node_t *curnode, *root=NULL, *node, *profile;
	wp_node *head, *tmp;
	struct list_head  *pos, *n;
	char buf[256]={0};
	FILE *fp;
	int ret = -1;

	if(!wplist) {
		mylog_error("%s:%d the wplist is NULL", __func__, __LINE__);
		return -1;
	}

	head = wplist;

	if ((fp = fopen(WIRELESS_PROFILE_XML, "w")) == NULL) {
		mylog_error("Failed to Create %s:%s\n", WIRELESS_PROFILE_XML, strerror(errno));
		ret = -1;
		goto end_save;
	}

	root = mxmlLoadString(MXML_ADD_TO_PARENT,
			MXML_HEAD_STRING, MXML_NO_CALLBACK);
	if (!root) {
		mylog_error("mxmlLoadString failed:%s\n", strerror(errno));
		ret = -1;
		goto end_save;
	}

	/* profile max num */
	if ((node = mxmlNewElement(MXML_NO_PARENT, TAG_PROFILE_MAX_NUM)) == NULL) {
		mylog_error("New Element node failed!\n");
		ret = -1;
		goto end_save;
	}
	sprintf(buf, "%d", profiles_max_num);
	mxmlElementSetAttr(node, TAG_ATTR_VALUE, buf);
	mxmlAdd(root, MXML_ADD_AFTER, root->last_child, node);

	list_for_each_safe(pos, n, &head->list){
		tmp = list_entry(pos, wp_node, list);
		snprintf(buf, sizeof(buf), "<%s>\n</%s>", TAG_PROFILE, TAG_PROFILE);
		profile = mxmlLoadString(root, buf, MXML_NO_CALLBACK);

		/* get the last profile node */
		for (curnode = node; curnode; curnode = curnode->next) {
			if (curnode->type != MXML_ELEMENT || 
					strcmp(curnode->value.element.name, TAG_PROFILE)) 
				continue;
			profile = curnode;
		}
		wireless_profile_create_xml_wp_content(tmp, profile);
	}
	mxmlSaveFile(root, fp, save_whitespace_cb);
	ret = 0;
end_save:
	if(fp) 
		fclose(fp);
	if (root) 
		mxmlDelete(root);

	return ret;
}

