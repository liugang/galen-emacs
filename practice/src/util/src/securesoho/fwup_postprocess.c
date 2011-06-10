/***************************************************************************
 * Copyright (C) 2006, Alphanetworks, inc. All Rights Reserved
 * @file:    fwup_postprocess.c
 * @date:    28/Dec/2006
 * @author:  wills_yin@alphanetworks.com
 * @brief    firmware update postprocessor source file.
 * @history:
 ***************************************************************************/
/*
 * Introduction
 *     For the ender user, it is a bad experience that all the user settings 
 *     would be lost after doing a firmware updating. This program will adapt
 *     all the orignal user settings to the new version config required by the
 *     new version firmware. 
 * How it works 
 *     This program would read a policy file, which specified a few rules about
 *     how to convert the old file into new file. For back compatibility, the 
 *     policy file should be wrote carefully.
 * The plolicy file format
 *     <?xml version="1.0" ?>
 *     <rules>
 *         <file default_policy="0" type="0" delimiter="=">
 *             <source>config_factory_default</source>
 *             <target>config</target>
 *             <itemset>
 *                 <item>
 *                     <label>FWUDSERVER</label>
 *                     <policy>1</policy>
 *                 </item> 
 *                 <item>
 *                     <label>CONFIG_TV_TRICK00</label>
 *                     <new>CONFIG_TV_TRICK</new>
 *                     <policy>2</policy>
 *                 </item> 
 *             </itemset>
 *         </file>
 *         <file default_policy="0" type="0" delimiter=":">
 *             <source>browser_config</source>
 *             <target>browser.txt</target>
 *             <itemset>
 *                 <item>
 *                     <label>larch.document.images.default</label>
 *                     <policy>1</policy>
 *             </itemset>
 *         </file>
 *         <file type="1">
 *             <source>file_to_be_added</source>
 *             <target>new_name</target>
 *         </file>
 *         <file type="2">
 *             <target>file_to_be_deleted</target>
 *         </file>
 *     </rules>
 * The policy type
 *     During the coverting process, there are some typical policies desevered
 *     to be mentioned:
 *     POLICY_DEFAULT 
 *         Keep the source item's label and vale, for all the new items in 
 *         target, just simplely copy the new one to source file.
 *     POLICY_UPDATE_VALUE
 *         Replace the source item's value  with target item's value; If missed
 *         in source file, the same as POLICY_DEFAULT
 *     POLICY_UPDATE_LABLE
 *         Replace the source item's labell with target item's label; If missed
 *         in souce file, the same as POLICY_DEFAULT
 *     POLICY_DISCLAIM
 *         The source item would be obsolete. You can keep it or delelte it.
 * The file processor type
 *     The file can be processed line by line or be processed as a whole file.
 *     You can extend all the others file processing type.
 *     FILE_PROCESS_LINE
 *         The line has the style: [label][delim][value]\n
 *     FILE_PROCESS_WHOLE_ADD
 *         Copy the file from source to target directory.        
 *     FILE_PROCESS_WHOLE_DEL
 *         Remove the file from target directory.
 * Note: 
 *     The source file or the target file can be absolute path or relative path.
 *     If relative, use the default directory.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <mxml.h>

#include "securesoho.h"

/*
 * For no mmu systemm, if the BUFFER_SIZE is too big, it may cause system crash randomly.
 */
#define BUFFER_SIZE          512

#define  XML_TAG_RULES       "rules"
#define  XML_TAG_FILE        "file"
#define  XML_ATTR_DEFPOLICY  "default_policy"
#define  XML_ATTR_DELIM      "delimiter"
#define  XML_ATTR_TYPE       "type"
#define  XML_TAG_SOURCE      "source"
#define  XML_TAG_TARGET      "target"
#define  XML_TAG_ITEMSET     "itemset"
#define  XML_TAG_ITEM        "item"
#define  XML_TAG_LABEL       "label"
#define  XML_TAG_NEWLABEL    "new"
#define  XML_TAG_POLICY      "policy"

#define  MULTIPLIER          37
#define  NHASH               101

#define SAFE_FREE( p ) \
        do {                         \
                if( p ) {            \
                        free( p );   \
                        (p) = NULL;  \
                }                    \
        } while(0)

#define DESTROY_LIST(_list_, _type_, _function_)            \
        do {                                                \
                _type_ *object = _list_;                    \
                while (object) {                            \
                        _type_ *object_to_destroy = object; \
                        object = object->next;              \
                        _function_(object_to_destroy);      \
                }                                           \
        } while (0)

enum {
	FILE_PROCESS_LINE = 0,
	FILE_PROCESS_WHOLE_ADD,
	FILE_PROCESS_WHOLE_DEL
};

enum {
	POLICY_DEFAULT = 0,
	POLICY_UPDATE_VALUE,
	POLICY_UPDATE_LABLE,
	POLICY_DISCLAIM
};

typedef struct config_item_t      config_item_t;
typedef struct item_rule_t        item_rule_t;
typedef struct file_rule_t        file_rule_t;
typedef struct fwup_rules_t       fwup_rules_t;

struct config_item_t {
	char*          label;
	char*          value;
	int            policy;
	int            obsolete;
	config_item_t* next;
};

struct item_rule_t {
	char*          label;
	char*          new;
	int            policy;
	item_rule_t*   next;
};

struct file_rule_t {
	int            type;
	char*          source;
	char*          target;
	char*          delim;
	int            default_policy;
	item_rule_t*   rule_list;
	file_rule_t*   next;
};

struct fwup_rules_t {
	int            num_file;
	file_rule_t*   file_list;
};

static config_item_t* item_hashtab[NHASH]; 

static int fwup_release_configitem(config_item_t* config)
{
	if (!config)
		return -1;

	SAFE_FREE(config->label);
	SAFE_FREE(config->value);
	free(config);

	return 0;
}

static int fwup_release_hashtab(void)
{
	int i;
	
	for (i = 0; i < NHASH; i++) {
		DESTROY_LIST(item_hashtab[i], config_item_t, fwup_release_configitem);
		item_hashtab[i] = NULL;
	}
	return 0;
}

static int fwup_release_ruleitem(item_rule_t* item)
{
	if (!item)
		return -1;

	SAFE_FREE(item->label);
	SAFE_FREE(item->new);
	free(item);

	return 0;
}

static int fwup_release_fileitem(file_rule_t* file)
{
	if (!file)
		return -1;

	SAFE_FREE(file->source);
	SAFE_FREE(file->target);
	SAFE_FREE(file->delim);
	DESTROY_LIST(file->rule_list, item_rule_t, fwup_release_ruleitem);

	free(file);

	return 0;
}

static int fwup_release_rules(fwup_rules_t* rules)
{
	if (!rules)
		return -1;

	DESTROY_LIST(rules->file_list, file_rule_t, fwup_release_fileitem);

	free(rules);

	return 0;
}

static unsigned int fwup_hash_string(const char *label)
{
	unsigned int h;
	unsigned char *p;

	h = 0;
	for (p = (unsigned char *) label; *p != '\0'; p++)
		h = h * MULTIPLIER + *p;
	return h % NHASH;
}

static config_item_t* fwup_lookup_hashtab(const char* label, int create)
{
	unsigned int h;
	config_item_t* sp;

	if ( !label )
		return NULL;

	h = fwup_hash_string(label);

	for (sp = item_hashtab[h]; sp != NULL; sp = sp->next) {
		if ( sp->label && !strcmp(label, sp->label) ) 
			return sp;
	}

	if ( create ) {
		sp = malloc(sizeof(config_item_t));
		if ( !sp ) {
			fprintf(stderr, "%s:%d, out of memory", __FILE__, __LINE__);
			return NULL;
		}
		bzero(sp, sizeof(config_item_t));
		sp->label  = strdup(label);
		sp->policy = POLICY_DEFAULT;
		sp->next   = item_hashtab[h];
		item_hashtab[h] = sp;
	}

	return sp;
}

/*
 * Get text from mxml Element node.Ex: <description>XX YY ZZ</description>
 */
static char *fwup_mxml_get_eletext( mxml_node_t *node )
{
	int  i, len;
	char *ret, *s;
	mxml_node_t *n;

	len = 0;	
	for(n = node->child; n ; n = n->next) {
		if (n->type == MXML_TEXT) {
			len += n->value.text.whitespace;
			len += strlen(n->value.text.string);
			len += 1;
		}
	}
	if (!len)
		return NULL;

	s = ret = (char *)malloc( len );
	if (!s) {
		printf("ERROR: malloc %d\n", len );
		return NULL;
	}
	
	for(n = node->child; n; n = n->next) {
		if (n->type == MXML_TEXT) {
			for(i = 0;i < n->value.text.whitespace; i++)
				*s++ = ' ';
			*s = '\0';
			strcat(ret, n->value.text.string);
			s += strlen(n->value.text.string);
			*s = '\0';
		}
	}
	return ret;
}

static int fwup_get_rulelist(mxml_node_t* node, item_rule_t** rule_list)
{
	char *str;
	const char *tag;
	item_rule_t *rule; 
	mxml_node_t *curnode, *childnode, *itemnode;

	if (!node || !rule_list) 
		return -1;

	*rule_list = NULL;

	itemnode = mxmlFindElement(node, node, XML_TAG_ITEM, NULL, NULL, MXML_DESCEND);

	for (curnode = itemnode; curnode; curnode = curnode->next) {
		rule = NULL;
		if (curnode->type != MXML_ELEMENT 
				|| strcmp(curnode->value.element.name, XML_TAG_ITEM))
			continue;

		if ((rule = malloc(sizeof(item_rule_t))) == NULL )
			continue;
		bzero(rule, sizeof(struct item_rule_t)); 

		for (childnode = curnode->child; childnode; childnode = childnode->next) {
			if (childnode->type != MXML_ELEMENT) 
				continue;

			tag = childnode->value.element.name;
			if (!tag) continue;
			str = fwup_mxml_get_eletext(childnode);
			if (!str) continue;

			if (!strcmp(tag, XML_TAG_LABEL)) 
				rule->label = strdup(str);
			if (!strcmp(tag, XML_TAG_NEWLABEL)) 
				rule->new = strdup(str);
			else if (!strcmp(tag, XML_TAG_POLICY))
				rule->policy = strtol(str, NULL, 10);

			free(str);
		}
		rule->next = *rule_list;
		*rule_list = rule;
	}

	return 0;
}

static int fwup_get_file_rulelist(mxml_node_t * root, fwup_rules_t* rules)
{
	const char *str, *tag;
	file_rule_t* file;
	mxml_node_t *curnode, *filenode, *childnode;

	if ( !root  || !rules )
		return -1;

	rules->file_list = NULL;

	filenode = mxmlFindElement(root, root, XML_TAG_FILE, NULL, NULL, MXML_DESCEND);

	for (curnode = filenode; curnode; curnode = curnode->next) {
		file = NULL;
		if (curnode->type != MXML_ELEMENT || 
				strcmp(curnode->value.element.name, XML_TAG_FILE)) 
			continue;

		if ((file = malloc(sizeof(file_rule_t))) == NULL)
			continue;
		bzero(file, sizeof(struct file_rule_t)); 

		str = mxmlElementGetAttr(curnode, XML_ATTR_DEFPOLICY);
		if (str) file->default_policy = strtol(str, NULL, 10);

		str = mxmlElementGetAttr(curnode, XML_ATTR_DELIM);
		if (str) file->delim = strdup(str);

		str = mxmlElementGetAttr(curnode, XML_ATTR_TYPE);
		if (str) file->type  = strtol(str, NULL, 10);


		for (childnode = curnode->child; childnode; childnode = childnode->next) {
			if (childnode->type != MXML_ELEMENT) 
				continue;

			tag = childnode->value.element.name;
			if (!tag)
				continue;

			if (!strcmp(tag, XML_TAG_SOURCE)) 
				file->source = fwup_mxml_get_eletext(childnode);
			else if (tag && !strcmp(tag, XML_TAG_TARGET))
				file->target = fwup_mxml_get_eletext(childnode);
			else if (tag && !strcmp(tag, XML_TAG_ITEMSET))
				fwup_get_rulelist(childnode, &file->rule_list);
		}

		file->next = rules->file_list;
		rules->file_list = file;

		rules->num_file++;
	}

	return 0;
}

static fwup_rules_t* fwup_parse_rule_file(const char *policy_file)
{
	FILE  *fp;
	mxml_node_t *root, *node;
	fwup_rules_t* rules;

	fp = NULL; root = NULL; rules = NULL;

	do {
		if ((fp = fopen(policy_file, "r")) == NULL) {
			fprintf(stderr, "Failed to Open :%s\n", strerror(errno));
			break;
		}

		if ( (root = mxmlLoadFile(MXML_NO_PARENT, fp, MXML_NO_CALLBACK)) == NULL) {
			fprintf(stderr, "%s:%d, %s, bad xml format.\n", 
					__FUNCTION__, __LINE__, policy_file);
			break;
		}

		if ( (rules = malloc(sizeof(fwup_rules_t))) == NULL ) {
			fprintf(stderr, "%s:%d, Out of memory.\n", 
					__FUNCTION__, __LINE__);
			break;
		}
		bzero(rules, sizeof(fwup_rules_t));

		node = mxmlFindElement(root, root, XML_TAG_RULES, NULL, NULL, MXML_DESCEND);
		if (!node) {
			fprintf(stderr, "%s:%d, No %s tag found in file.\n", 
					__FUNCTION__, __LINE__, XML_TAG_RULES);
			free(rules);
			rules = NULL;
			break;
		}

		fwup_get_file_rulelist(node, rules);

	} while(0);

	if (fp) fclose(fp);
	if (root) mxmlDelete(root);

	return rules;
}

static int chomp(char* buf)
{
	char *p, *q;

	if (!buf || !*buf )
		return -1;
	p = buf;
	while( *p && isspace(*p) ) p++;
	if (*p == '\0') {
		*buf = '\0';
		return 0;
	} 

	if ( p != buf)
		memmove(buf, p, strlen(p) + 1);

	q = buf + strlen(buf) - 1;
	while( *q && isspace(*q)  && q >= buf) q--;
	*(q+1) = '\0';

	return 0;
}

static int fwup_parse_keypairs(char *buf, char **key, char **value, const char *delim)
{
	char *p;

	if ( !key || !value || buf[0] == '\0' || !delim)
		return -1;

	*key = *value = NULL;
	if ( (p = strstr(buf, delim)) == NULL) {
		chomp(*key);
		*value = NULL;
	} else {
		*p = '\0';
		*key = buf;
		chomp(*key);
		(*value) = p + strlen(delim);
		chomp(*value);
	}
	return 0;
}

static int fwup_import_target(file_rule_t *file)
{
	FILE *fp;
	char	buf[BUFFER_SIZE];
	char *label, *value;
	config_item_t *item;

	if (!file || !file->target)
		return -1;

	fp = NULL;
	if ( (fp = fopen(file->target, "r")) == NULL ) {
		fprintf(stderr, "%s, %d,Failed to Open %s:%s\n", 
				__FUNCTION__, __LINE__, buf, strerror(errno));
		return -1;
	}

	while (fgets(buf, sizeof(buf), fp)) {
		label = value = NULL;
		fwup_parse_keypairs( buf, &label, &value, file->delim);
		if (!label || !value)
			continue;
		item = fwup_lookup_hashtab(label, 1);
		if (item) {
			SAFE_FREE(item->value);
			item->value = strdup(value);
		}
	}
	fclose(fp);

	return 0;
}

static int fwup_apply_rules(file_rule_t *file)
{
	item_rule_t   *rule;
	config_item_t *item, *newitem;

	if (!file)
		return -1;

	for (rule = file->rule_list; rule; rule = rule->next) {
		item = fwup_lookup_hashtab(rule->label, 1);
		if (!item) 
			continue;
		item->policy = rule->policy;

		switch (item->policy) {
		case POLICY_UPDATE_LABLE :
			if (rule->new) {
				/* set the older item to obsoleted */
				item->obsolete = 1;
				/* seek the new lable */
				newitem = fwup_lookup_hashtab(rule->new, 0);
				/* create a new one if we don't found a new one*/
				if((NULL == newitem) && 
				   ((newitem = fwup_lookup_hashtab(rule->new, 1)))){
					newitem->value = item->value?strdup(item->value):NULL;
				}
			}
			break;
		case POLICY_DISCLAIM:
			item->obsolete = 1;
			break;
		case POLICY_DEFAULT:
			break;
		}
	}

	return 0;
}

static int fwup_merge_source(file_rule_t *file)
{
	FILE *fp;
	config_item_t *item;
	char	buf[BUFFER_SIZE];
	char *label, *value;

	if (!file || !file->source)
		return -1;

	if ( (fp = fopen(file->source, "r")) == NULL ) {
		fprintf(stderr, "%s, %d,Failed to Open %s:%s\n", 
				__FUNCTION__, __LINE__, buf, strerror(errno));
		return -1;
	}

	while (fgets(buf, sizeof(buf), fp)) {
		fwup_parse_keypairs( buf, &label, &value, file->delim);
		if (!label || !value)
			continue;
		item = fwup_lookup_hashtab(label, 0);
		if (!item){
			/* it's new added item */
			item = fwup_lookup_hashtab(label, 1);
			if(item){
				SAFE_FREE(item->value);
				item->value = strdup(value);
			}else{
				continue;
			}
		}

		switch(item->policy) {
		case POLICY_UPDATE_VALUE:
			SAFE_FREE(item->value);
			item->value = strdup(value);
			break;
		default:
			break;
		}
	}
	fclose(fp);

	return 0;
}

static int fwup_writeback_target(file_rule_t* file)
{
	int i;
	FILE *fp;
	char buf[BUFFER_SIZE];
	config_item_t *item;

	if (!file)
		return -1;

	if ( (fp = fopen(file->target, "w")) == NULL ) {
		fprintf(stderr, "%s, %d,Failed to Open %s:%s\n", 
				__FUNCTION__, __LINE__, buf, strerror(errno));
		return -1;
	}

	for (i = 0; i < NHASH; i++) {
		for (item = item_hashtab[i]; item; item = item->next) {
			if ( !item->label || !item->value) 
				continue;
			if ( item->obsolete )
				continue;
			fprintf(fp, "%s%s%s\n", item->label, file->delim, item->value);
		}
	}
	
	fclose(fp);
	return 0;
}

static int fwup_process_whole_file(file_rule_t *file, int add)
{
	if (!file)
		return -1;

	if (add) {
		if (!file->target || file->target[0] == '\0'){
			//snprintf(buf, sizeof(buf), "/bin/cp -f %s %s", file->source, file->source);
			securesoho_copy(file->source, file->target);
		}else{
			//snprintf(buf, sizeof(buf), "/bin/cp -f %s %s", file->source, file->target);
			securesoho_copy(file->source, file->target);
		}
	} else {
		if (!file->target || file->target[0] == '\0')
			return -1;
		securesoho_remove(file->target);
		//snprintf(buf, sizeof(buf), "/bin/rm -f %s", file->target);
	}
	return 0;
}

static int fwup_process_filerules(file_rule_t *file)
{
	struct stat st;

	if (!file)
		return -1;
	if (!file->source || stat(file->source, &st))
		return -1;

	switch (file->type) {
	case FILE_PROCESS_LINE:
		if (!file->target || stat(file->target, &st)) {
			fwup_process_whole_file(file, 1);
			return 0;
		}
		bzero(&item_hashtab[0], sizeof(item_hashtab));

		if (fwup_import_target(file) == -1)
			return -1;
		fwup_apply_rules(file);

		fwup_merge_source(file);

		fwup_writeback_target(file);

		fwup_release_hashtab();
		break;
	case FILE_PROCESS_WHOLE_ADD:
		fwup_process_whole_file(file, 1);
		break;
	case FILE_PROCESS_WHOLE_DEL:
		fwup_process_whole_file(file, 0);
		break;
	default:
		break;
	}

	return 0;
}

int fwup_post_process(const char *policy_file)
{
	file_rule_t*  p;
	fwup_rules_t* rules;
	struct stat st;

	if((NULL == policy_file) || stat(policy_file, &st)){
		return -1;
	}
	if ( (rules = fwup_parse_rule_file(policy_file)) == NULL)
		return -1;

	p = rules->file_list;
	while (p) {
		fwup_process_filerules(p);
		p = p->next;
	}
	fwup_release_rules(rules);

	return 0;
}

#if 0
int main(int argc, char* argv[])
{
	fwup_post_process();
	return 0;
}
#endif
