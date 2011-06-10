#ifndef __FLASH_OP_H__
#define __FLASH_OP_H__

typedef void (*flash_erase_callback_fn)(void *data);

typedef struct flash_op_s{
	int (*write)(char *p_dev, unsigned char *p_buff, size_t size);
	int (*erase)(char *p_dev, flash_erase_callback_fn cb);
	int (*erase_range)(char *p_dev, size_t start_pos, size_t fsize, flash_erase_callback_fn cb);
	int (*get_flash_block_count)(char *p_dev);
} flash_op_t;

extern flash_op_t *create_flash_op();
extern void destroy_flash_op(flash_op_t **p_flash_op);

/*
 * API: for flash_op write
 */
static inline int flash_op_write(flash_op_t *p_flash_op, char *p_dev, unsigned char *p_buff, size_t size)
{
	if(p_flash_op && p_flash_op->write) {
		return p_flash_op->write(p_dev, p_buff, size);
	} else {
		return 0;
	}
}

/*
 * API: for flash_op erase
 */
static inline int flash_op_erase(flash_op_t *p_flash_op, char *p_dev, flash_erase_callback_fn cb)
{
	if(p_flash_op && p_flash_op->erase) {
		return p_flash_op->erase(p_dev, cb);
	} else {
		return 0;
	}
}

/*
 * API: for flash_op erase_range
 */
static inline int flash_op_erase_range(flash_op_t *p_flash_op, char *p_dev, size_t start_pos, size_t fsize, flash_erase_callback_fn cb)
{
	if(p_flash_op && p_flash_op->erase_range) {
		return p_flash_op->erase_range(p_dev, start_pos, fsize, cb);
	} else {
		return 0;
	}
}

/*
 * API: for flash_op get flash block count
 */
static inline int get_flash_block_count(flash_op_t *p_flash_op ,char *p_dev)
{
	if(p_flash_op && p_flash_op->get_flash_block_count) {
		return p_flash_op->get_flash_block_count(p_dev);
	} else {
		return 0;
	}
}
#endif //__FLASH_OP_H__
