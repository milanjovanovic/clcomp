#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lispo.h"


struct cons *allocate_cons(void **heap, lispobj car, lispobj cdr) {
  struct cons *cons = (struct cons *) *heap;
  *heap += sizeof(struct cons);
  cons->car = car;
  cons->cdr = cdr;
  return cons;
}

lispobj tag_cons(struct cons *cons) {
  return (lispobj) cons | CONS_TAG;
}

struct cons *untag_cons(lispobj cons) {
  return (struct cons *) untag_pointer(cons);
}

lispobj car(lispobj lisp_cons) {
  struct cons *cons = untag_cons(lisp_cons);
  return cons->car;
}

lispobj cdr(lispobj lisp_cons) {
  struct cons *cons = untag_cons(lisp_cons);
  return cons->cdr;
}

struct symbol *untag_symbol(lispobj cons) {
  return (struct symbol *) untag_pointer(cons);
}

lispobj symbol_name(lispobj symbol) {
  struct symbol *sym = untag_symbol(symbol);
  return sym->name;
}

lispobj symbol_value(lispobj symbol) {
  struct symbol *sym = untag_symbol(symbol);
  return sym->value;
}

lispobj symbol_function(lispobj symbol) {
  struct symbol *sym = untag_symbol(symbol);
  return sym->function;
}

lispobj symbol_plist(lispobj symbol) {
  struct symbol *sym = untag_symbol(symbol);
  return sym->plist;
}

int is_symbol_interned(lispobj symbol, lispobj env_array) {
  // FIXME
  struct array *env = (struct array *) env_array;
  return 0;
}

struct array *allocate_string(void **heap, char *cstring) {

  struct array *lisp_str = (struct array *) *heap;
  int cstr_size = strlen(cstring);

  //  FIXME
  //  lisp_str->tag = EXTENDED_TAG_STRING;
  lisp_str->size = cstr_size;
  
  *heap += sizeof(struct array);
  *heap -= WORD_SIZE;
  
  for(int i = 0; i < cstr_size; i++) {
    uintptr_t word_char = cstring[i];
    word_char <<= 8;
    word_char |= CHAR_TAG;
    memcpy(*heap, &word_char, WORD_SIZE);
    *heap += WORD_SIZE;
  }

  return lisp_str;
}

lispobj tag_array(struct array *array) {
  return (lispobj) array | POINTER_TAG;
}
  

lispobj tag_fixnum(int64_t obj) {

  // don't just shift, check for sign bit first
  unsigned long mask = 0x8000000000000000UL;
  unsigned long negative = mask & obj;
  
  long c = obj << TAG_SIZE;
  
  if(negative) {
    c |= mask;
  }
  
  return c;
}

int64_t untag_fixnum(lispobj obj) {
  
   // don't just shift, check for sign bit first
  uint64_t mask = 0x8000000000000000UL;
  uint64_t negative = mask & obj;

  int64_t c = obj >> TAG_SIZE;
  
  if(negative) {
    c |= 0xF000000000000000UL;
  }
  
  return c;
}

lispobj tag_char(char c) {
  lispobj lc = 0L;
  lc |= c;
  lc <<= TAG_SIZE;
  lc |= CHAR_TAG;
  return lc;
}

char untag_char(lispobj obj) {
  obj >>= TAG_SIZE;
  return (char) obj;
}

lispobj tag_pointer(uintptr_t pointer) {
  return pointer | POINTER_TAG;
}

uintptr_t untag_pointer(lispobj obj) {
  return obj & CLEAR_TAG_MASK;
}
