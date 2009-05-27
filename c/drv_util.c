#include <string.h>
#include <erl_driver.h>


#include "drv_util.h"

inline int read_int32(const char* data, int offset) {
  return ((((int)(((unsigned char*) (data))[offset]))  << 24) |
	  (((int)(((unsigned char*) (data))[offset + 1]))  << 16) |
	  (((int)(((unsigned char*) (data))[offset + 2]))  << 8)  |
	  (((int)(((unsigned char*) (data))[offset + 3]))));
}

// Any string read using this function must be freed
// using driver_free later
char* read_string(const char* data, int offset, int length) {
  char* buf = (char*) driver_alloc(length);
  const char* source = &data[offset];
  memcpy(buf, source, length);
  return buf;
}
