#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lispo.h"


struct cons *allocate_lisp_cons(void **heap, lispobj car, lispobj cdr) {
  struct cons *cons = (struct cons *) *heap;
  *heap += sizeof(struct cons);
  cons->car = car;
  cons->cdr = cdr;
  return cons;
}

struct array *allocate_lisp_string(void **heap, char *cstring) {

  uintptr_t start = (uintptr_t) *heap;
  
  struct array *lisp_str = (struct array *) *heap;
  int cs_size = strlen(cstring);
  
  *heap += sizeof(struct array);
  
  *heap -= WORD_SIZE;

  strncpy(*heap, cstring, cs_size);

  *heap += cs_size;
  // FIXME, change to right ARRA& STRING TAG
  lisp_str->tag = 0;
  lisp_str->size = cs_size;

  uintptr_t end = (uintptr_t) *heap;
  uintptr_t allocated = (uintptr_t) *heap - start;

  int diff = WORD_SIZE - ((uintptr_t) *heap % WORD_SIZE);
  if(diff > 0) {
    memset(*heap, 0, diff);
  }

  *heap += diff;

  return lisp_str;
}
