#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
static int saved_stdout_fd = -1;
static fpos_t saved_stdout_pos;
static int saved_stderr_fd = -1;
static fpos_t saved_stderr_pos;

void redirect_stdout(char *filename)
{
	fflush(stdout);
	fgetpos(stdout, &saved_stdout_pos);
	saved_stdout_fd = dup(fileno(stdout));
	freopen(filename, "a", stdout);
}

void redirect_stdout_back(void)
{
	if (saved_stdout_fd < 0)
		return;
	fflush(stdout);
	dup2(saved_stdout_fd, fileno(stdout));
	close(saved_stdout_fd);
	saved_stdout_fd = -1;
	clearerr(stdout);
	fsetpos(stdout, &saved_stdout_pos);  
}

void redirect_stderr(char *filename)
{
	fflush(stderr);
	fgetpos(stderr, &saved_stderr_pos);
	saved_stderr_fd = dup(fileno(stderr));
	freopen(filename, "a", stderr);
}

void redirect_stderr_back(void)
{
	if (saved_stderr_fd < 0)
		return;
	fflush(stderr);
	dup2(saved_stderr_fd, fileno(stderr));
	close(saved_stderr_fd);
	saved_stderr_fd = -1;
	clearerr(stderr);
	fsetpos(stderr, &saved_stderr_pos);  
}
