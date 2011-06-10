/*
 * vim:cindent:ts=8
 */
#ifndef	__CONFIG_ACCESS_H__
#define	__CONFIG_ACCESS_H__

enum TUPLE_TYPE{
	TUPLE_UNKNOWN=0,
	TUPLE_STRING,
	TUPLE_INT,
	TUPLE_STRING_DUP
};

struct	tuple_s	{
	char	*label;
	void	*container;
	enum TUPLE_TYPE   type;
};

typedef	struct	tuple_s	tuple_t;

#define SOURCE_DIR      	        "/tmp/conf"
#define MP_SOURCE_DIR      	        "/tmp/mp"

#define	SECURESOHO_CONFIG		"/conf/config"
#define	SECURESOHO_TMP_CONFIG		"/tmp/config"
#define	SECURESOHO_CONFIG_LOCK		"/tmp/config_lock"

#define BROWSER_CONFIG_TXT		"/tmp/conf/browser_config.txt"
#define DMA_CONFIG_FILE			"/tmp/conf/config"

/* basic config access utility */
int bs_config_lock(const char *lock);
int bs_config_unlock(int fd);
int bs_config_string_get(const char *file, const char *label, char *container);
int bs_config_string_set(const char *file, const char *label, const char *container);
int bs_config_int_get(const char *file, const char *label);
int bs_config_int_set(const char *file, const char *label, int value);

/* securesoho config access utiliy */
extern	int	securesoho_config_lock(void);
extern	void	securesoho_config_unlock(int fd);

extern	int	securesoho_values_set( tuple_t *);
extern	int	securesoho_values_get( tuple_t *);
extern	void	securesoho_string_get( const char *label, char *container);
extern	int	securesoho_string_set( const char *label, const char *target );
extern	int	securesoho_int_get( const char *label );
extern	int	securesoho_int_set( const char *label, int val );

extern	void	securesoho_replace_config( char *path );

extern  void    securesoho_parse_value(char *buf, char *key, char *value);
extern	int	safe_rename( char *, char *);
extern	int	safe_cp( char *, char *);

#endif
