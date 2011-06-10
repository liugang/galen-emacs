#ifndef  COBJECT_H
#define  COBJECT_H

#include  "cobject_defs.h"

struct cobject_type_s {
	const cobject_type_t *base;
	const char *name;
	size_t size_of_object;
	void (* destruct)(cobject_t *self);
};

struct cobject_s {
	const cobject_type_t *type;
	size_t refcnt;
};

extern const cobject_type_t cobject_class;

cobject_t *cobject(const cobject_type_t *type);
void cobject_destruct_v(cobject_t *self);

bool cobject_is_instance(const cobject_t *self,
			 const cobject_type_t *type);
#define COBJECT_IS_INSTANCE(self, class)			\
	cobject_is_instance((const cobject_t *)(self),		\
			    (const cobject_type_t *)&(class))

void cobject_incref(cobject_t *self);
void cobject_decref(cobject_t *self);

#endif
