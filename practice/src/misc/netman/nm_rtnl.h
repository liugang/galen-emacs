#ifndef _NM_RTNL_h__
#define _NM_RTNL_h__

/**
 */
struct nm_rtnl_handle {
	int sk;
};

int nm_rtnl_handle_init(struct nm_rtnl_handle *rtnl);
int nm_rtnl_handle_fini(struct nm_rtnl_handle *rtnl);
int nm_rtnl_handle_get_fd(struct nm_rtnl_handle *rtnl);
int nm_rtnl_handle_events(struct nm_rtnl_handle *rtnl);

#endif /* _NM_RTNL_h__ */
