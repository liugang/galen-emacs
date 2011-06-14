#include <stdio.h>

int main(int argc, char *argv[])
{
	int i, *pi = &i;

	printf("address of i = 0x%p, value of pi = 0x%p\n", &i, pi);
	return 0;
}
