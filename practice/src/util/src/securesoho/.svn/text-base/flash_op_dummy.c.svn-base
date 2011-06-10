#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "flash_op.h"

static int dummy_flash_write(char *p_dev, unsigned char *p_buff, size_t size)
{	
	return 0;
}

static int dummy_flash_erase(char *p_dev, flash_erase_callback_fn cb)
{	
	return 0;
}

static flash_op_t *  dummy_create_flash_op()
{
	flash_op_t *p_this = NULL;

	p_this = (flash_op_t *) malloc(sizeof(flash_op_t));
	if (!p_this) return NULL;
	else	memset(p_this, 0, sizeof(flash_op_t));

	p_this->write = dummy_flash_write;
	p_this->erase = dummy_flash_erase;
	
	return p_this;
}

static void dummy_destroy_flash_op(flash_op_t *p_flash_op)
{
	flash_op_t *p_this = p_flash_op;

	if (p_this) free(p_this);
}

/*
 * Public API: create_flash_op, just create a flash_op object
 */
extern flash_op_t *create_flash_op()
{
	flash_op_t *p_this = NULL;
	p_this =  dummy_create_flash_op();
	return p_this;
}

/*
 * Public API: destroy_flash_op, just delete a flash_op object
 */
extern void destroy_flash_op(flash_op_t **p_flash_op)
{
	dummy_destroy_flash_op(*p_flash_op);
	if(*p_flash_op) *p_flash_op = NULL;
}




