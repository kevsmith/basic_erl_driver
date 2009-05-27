/*
Copyright (c) 2009 Hypothetical Labs, Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#include <string.h>
#include <erl_driver.h>
#include <ei.h>

#include "config.h"
#include "driver_comm.h"
#include "smaz.h"

typedef struct _basic_drv_t {
  ErlDrvPort port;
} basic_drv_t;


static ErlDrvData start(ErlDrvPort port, char* cmd);
static void stop(ErlDrvData handle);
static void process(ErlDrvData handle, ErlIOVec *ev);

static void do_echo(basic_drv_t *driver_data, char *data);
static void do_reverse(basic_drv_t *driver_data, char *data);
static void do_smaz_compress(basic_drv_t *driver_data, char *data);
static void do_smaz_decompress(basic_drv_t *driver_data, char *data);
static void send_error(basic_drv_t *driver_data);
static void send_output(ErlDrvPort port, ErlDrvTermData *terms, int term_count);

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
  basic_drv_t *retval = (basic_drv_t*) driver_alloc(sizeof(basic_drv_t));
  retval->port = port;
  return (ErlDrvData) retval;
}

static void stop(ErlDrvData handle) {
  basic_drv_t *driver_data = (basic_drv_t*) handle;
  driver_free(driver_data);
}

static void process(ErlDrvData handle, ErlIOVec *ev) {
  basic_drv_t *driver_data = (basic_drv_t*) handle;
  ErlDrvBinary *args = ev->binv[1];
  char *data = args->orig_bytes;
  char *command = read_command(&data);
  if (strncmp(command, "rv", 2) == 0) {
    do_reverse(driver_data, data);
  }
  else if (strncmp(command, "ec", 2) == 0) {
    do_echo(driver_data, data);
  }
  else if (strncmp(command, "sc", 2) == 0) {
    do_smaz_compress(driver_data, data);
  }
  else if (strncmp(command, "sd", 2) == 0) {
    do_smaz_decompress(driver_data, data);
  }
  else {
    send_error(driver_data);
  }
  driver_free(command);
}

static void do_echo(basic_drv_t *driver_data, char *data) {
  char *text = read_string(&data);
  ErlDrvTermData terms[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
			   ERL_DRV_BUF2BINARY, text, strlen(text),
			   ERL_DRV_TUPLE, 2};
  send_output(driver_data->port, terms, sizeof(terms) / sizeof(terms[0]));
}

static char *reverse_str(char* str) {
  int i = 0;
  int j = 0;
  int len = 0;
  char temp;
  char *ptr = NULL;

  len = strlen(str);
  ptr = driver_alloc(len + 1);
  memset(ptr, 0, len + 1);
  ptr = strcpy(ptr,str);
  for (i=0, j=len-1; i<=j; i++, j--) {
    temp = ptr[i];
    ptr[i] = ptr[j];
    ptr[j] = temp;
  }
  return ptr;
}

static void do_reverse(basic_drv_t *driver_data, char *data) {
  char *text = read_string(&data);
  char *rev_text = reverse_str(text);
  ErlDrvTermData terms[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
			    ERL_DRV_BUF2BINARY, rev_text, strlen(rev_text),
			    ERL_DRV_TUPLE, 2};
  send_output(driver_data->port, terms, sizeof(terms) / sizeof(terms[0]));
  driver_free(text);
  driver_free(rev_text);
}

static void do_smaz_compress(basic_drv_t *driver_data, char *data) {
  char *text = read_string(&data);
  int bufsize = strlen(text);
  int result = 0;
  char *buf = (char *) driver_alloc(bufsize + 1);
  memset(buf, 0, bufsize + 1);
  result = smaz_compress(text, bufsize, buf, bufsize);
  if (result > bufsize) {
    ErlDrvTermData terms[] = {ERL_DRV_ATOM, driver_mk_atom("error"),
			       ERL_DRV_ATOM, driver_mk_atom("compress_expansion"),
			       ERL_DRV_TUPLE, 2};
    send_output(driver_data->port, terms, sizeof(terms) / sizeof(terms[0]));
  }
  else {
    ErlDrvTermData terms[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
			      ERL_DRV_BUF2BINARY, buf, result,
			      ERL_DRV_TUPLE, 2};
    send_output(driver_data->port, terms, sizeof(terms) / sizeof(terms[0]));
  }
  driver_free(text);
  driver_free(buf);
}

static void do_smaz_decompress(basic_drv_t *driver_data, char *data) {
  char *text = read_string(&data);
  int bufsize = strlen(text) * 2;
  int result = 0;
  char *buf = (char *) driver_alloc(bufsize + 1);
  memset(buf, 0, bufsize + 1);
  result = smaz_decompress(text, strlen(text), buf, bufsize);
  if (result > bufsize) {
    ErlDrvTermData terms[] = {ERL_DRV_ATOM, driver_mk_atom("error"),
			       ERL_DRV_ATOM, driver_mk_atom("no_space"),
			       ERL_DRV_TUPLE, 2};
    send_output(driver_data->port, terms, sizeof(terms) / sizeof(terms[0]));
  }
  else {
    ErlDrvTermData terms[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
				ERL_DRV_BUF2BINARY, buf, result,
				ERL_DRV_TUPLE, 2};
    send_output(driver_data->port, terms, sizeof(terms) / sizeof(terms[0]));
  }
  driver_free(text);
  driver_free(buf);
}

static void send_error(basic_drv_t *driver_data) {
  ErlDrvTermData terms[] = {ERL_DRV_ATOM, driver_mk_atom("error"),
			    ERL_DRV_ATOM, driver_mk_atom("unknown_command"),
			    ERL_DRV_TUPLE, 2};
  send_output(driver_data->port, terms, sizeof(terms) / sizeof(terms[0]));
}

static void send_output(ErlDrvPort port, ErlDrvTermData *terms, int term_count) {
  driver_output_term(port, terms, term_count);
}
