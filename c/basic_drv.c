#include <erl_driver.h>
#include <ei.h>

#include "config.h"
#include "drv_util.h"

typedef struct _basic_drv_t {
  ErlDrvPort port;
} basic_drv_t;


static ErlDrvData start(ErlDrvPort port, char* cmd);
static void stop(ErlDrvData handle);
static void process(ErlDrvData handle, ErlIOVec *ev);

static ErlDrvEntry basic_driver_entry = {
    NULL,                             /* init */
    start,                            /* startup */
    stop,                             /* shutdown */
    NULL,                             /* output */
    NULL,                             /* ready_input */
    NULL,                             /* ready_output */
    "basic_drv",                        /* the name of the driver */
    NULL,                             /* finish */
    NULL,                             /* handle */
    NULL,                             /* control */
    NULL,                             /* timeout */
    process,                          /* process */
    NULL,                             /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING     /* ERL_DRV_FLAGs */
};

DRIVER_INIT(basic_driver) {
  return &basic_driver_entry;
}

static ErlDrvData start(ErlDrvPort port, char* cmd) {
  basic_drv_t* retval = (basic_drv_t*) driver_alloc(sizeof(basic_drv_t));
  retval->port = port;
  return (ErlDrvData) retval;
}

static void stop(ErlDrvData handle) {
  basic_drv_t* driver_data = (basic_drv_t*) handle;
  driver_free(driver_data);
}

static void process(ErlDrvData handle, ErlIOVec *ev) {
  basic_drv_t* driver_data = (basic_drv_t*) handle;
  ErlDrvBinary* data = ev->binv[1];
  int size = read_int32(data->orig_bytes, 0);
  char *s = read_string(data->orig_bytes, 4, size);
  printf("Data: %s\n", s);
  ErlDrvTermData spec[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
			   ERL_DRV_BINARY, (ErlDrvTermData) data, data->orig_size, 0,
			   ERL_DRV_TUPLE, 2};
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}
