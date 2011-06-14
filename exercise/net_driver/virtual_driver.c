#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/init.h>
#include <linux/version.h>
#include <linux/string.h>
#include <linux/list.h>

typedef struct net_device	* PNET_DEV;

MODULE_LICENSE("GPL");
MODULE_AUTHOR("galen");

struct module *m = &__this_module;

int print_module_test(void)
{
    struct module *mod;

    list_for_each_entry(mod, &m->list, list) {
	printk("%s\n", mod->name);
    }

    return 0;
}

static int list_print_init(void)
{
    printk("load list_print module.\n");
    print_module_test();
    return 0;
}

static void list_print_exit(void)
{
    printk("unload list_print module.\n");
}

module_init(list_print_init);
module_exit(list_print_exit);
