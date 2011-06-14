 #include <stdio.h>
 
 #define MYLOG_CATEGORY_NAME "log4c.examples.helloworld"
 #include "mylog.h"
 
int main(int argc, char** argv){
	int rc = 0;

	if (mylog_init()){
		printf("mylog_init() failed");
		rc = 1;  
	}else{

		mylog_fatal("Hello, world at line %d", __LINE__);
		mylog_alert("Hello, world at line %d", __LINE__);
		mylog_error("Hello, world at line %d", __LINE__);
		mylog_warn("Hello, world at line %d", __LINE__);
		mylog_debug("Hello, world at line %d", __LINE__);
		mylog_info("Hello, world at line %d", __LINE__);
		mylog_trace("Hello, world at line %d", __LINE__);

		/* Explicitly call the log4c cleanup routine */
		if ( mylog_fini()){
			printf("mylog_fini() failed");
		}
	}
	return rc;
}

