/**
* @file   i2c_ei.c
* @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
* @brief  I2C erlang interface
* @description
*
* @section LICENSE
* Copyright (C) 2013 Erlang Solutions Ltd.
**/

#include <stdio.h>
#include <stdlib.h>
#include "erl_interface.h"
#include "ei.h"

/*! \addtogroup I2C
*  @brief I2C library functions
*  @{
*/

typedef unsigned char byte;

/** TX/RX data buffer */
unsigned char *data;

/**
* @brief Converts an erlang tuple in an C array and puts the data into the *data variable.
* @param The erlang tuple.
*/
void tuple_to_array(ETERM *tuple) {

  int tplsize = ERL_TUPLE_SIZE(tuple);

  data=(unsigned char *) malloc (tplsize * sizeof (char));
  int i=0;
  for (i=0; i<tplsize; i++) {
    data[i]=(char) ERL_INT_VALUE(erl_element(i+1, tuple));
  }
}

/**
* @brief The main function.
* It waits for data in the buffer and calls the driver.
*/

int main() {

  ETERM *tuplep, *intp;
  ETERM *fnp;
  int res;
  byte buf[100];

  int fd;

  erl_init(NULL, 0);

  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    
    // calls the i2c_init_name function and returns the fd or -1
    if (strcmp(ERL_ATOM_PTR(fnp), "i2c_init", 8) == 0) {
      res = i2c_init_name(erl_iolist_to_string(erl_element(2, tuplep)));
      fd=res;
      if (res < 0) {
	intp = erl_mk_int(-1);
      }
      else {
	intp = erl_mk_int(res);
      }
    }
    // calls the i2c_write function and returns 1 if success or -1 if fails
    else if (strcmp(ERL_ATOM_PTR(fnp), "i2c_write", 9) == 0) {
      tuple_to_array(erl_element(3, tuplep));
      res = i2c_write(fd, ERL_INT_VALUE(erl_element(2, tuplep)), data, ERL_INT_VALUE(erl_element(4, tuplep)));
      intp = erl_mk_int(res);
    }
    // calls the i2c_read function and returns an erlang tuple with data or -1 if fails
    else if (strncmp(ERL_ATOM_PTR(fnp), "i2c_read", 8) == 0) {
      int size = ERL_INT_VALUE(erl_element(3, tuplep));
      data=(unsigned char *) malloc (size * sizeof (char));

      res = i2c_read(fd, ERL_INT_VALUE(erl_element(2, tuplep)), data, size);

      if (res < 0) {
	intp=erl_mk_int(-1);
      }
      // converts data array in an erlang tuple
      else {       
	ETERM *etermarray[size];
	int i=0;
	for (i=0; i<size; i++) {
	  etermarray[i]=erl_mk_int(data[i]);
	}
	intp=erl_mk_tuple(etermarray, size);
      }
    }
    
    // converts the result in the Erlang external term format
    erl_encode(intp, buf);

    // sends the result to erlang
    write_cmd(buf, erl_term_len(intp));

    erl_free_compound(tuplep);
    erl_free_term(fnp);
    erl_free_term(intp);
  }
  return 1;
}
