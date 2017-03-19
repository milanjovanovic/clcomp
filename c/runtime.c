#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <dlfcn.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include "lispo.h"


#define STACK_SIZE 2097152 // 2 MB
#define HEAP_SIZE 52428800 // 50 MB
#define HEAP_START 0x200000000
#define STACK (HEAP_START + (2 * 1024 * 1024))

lispobj entry(uintptr_t stack, uintptr_t heap);

uintptr_t heap = HEAP_START;
uintptr_t stack = STACK;
uintptr_t current_heap = HEAP_START;

int is_pointer(lispobj obj) {
  return (obj & MASK) == POINTER_TAG ? 1 : 0;
}

int is_char(lispobj obj) {
  return (obj & MASK) == CHAR_TAG ? 1 : 0;
}

int is_fixnum(lispobj obj) {
  return (obj & MASK) == FIXNUM_TAG ? 1 : 0;
}

int is_list(lispobj obj) {
  return (obj & MASK) == LIST_TAG ? 1 : 0;
}
    
int is_fun(lispobj obj) {
  return (obj & MASK) == FUNCTION_TAG ? 1 : 0;
}

enum base_lisp_type get_lisp_type(lispobj obj) {
  
  if (is_fixnum(obj)) {
    return FIXNUM;
  } else if (is_char(obj)) {
    return CHAR;
  } else if (is_pointer(obj)) {
    return POINTER;
  } else if (is_list(obj)) {
    return LIST;
  } else if (is_fun(obj)) {
    return FUNCTION;
  } else {
    return -1;
  }
}

void print_lisp(lispobj obj) {
  
  enum base_lisp_type type = get_lisp_type(obj);
  
  switch(type) {
    
  case FIXNUM :
    // FIXME, this don't work for negative fixnums
    printf("FIXNUM: %lld\n", (int64_t) obj >> TAG_SIZE);
    break;
  case CHAR :
    printf("CHAR: %c\n", (char) ((obj >> 8) << 1));
    break;
  case LIST :
    printf("LIST\n");
    break;
  case FUNCTION :
    printf("FUNCTION\n");
    break;
  default :
    printf("UNKNOWN PRINT: %d", type);
    break;
  }
}

void *allocate_heap() {
  return mmap((void *) HEAP_START,
	      HEAP_SIZE,
	      PROT_READ | PROT_WRITE | PROT_EXEC,
	      MAP_PRIVATE | MAP_ANON | MAP_FIXED, -1, 0);
}
  
void copy_code(void *code, int size) {
  //  memcpy((void *) heap, code, size);
}

int main() {
  
  //  print_lisp(entry(stack, heap));
  void *p = allocate_heap();
  printf("heap start: %p\n", p);

  
  //printf("%p\n", p);
  //struct cons *cons = allocate_lisp_cons(&p, (lispobj) 1, (lispobj) 2);
  //printf("%p\n", p);
  //printf("%lu\n", cons->cdr);

  char *str = "abcd";
  struct array *lisp_str = allocate_lisp_string(&p, str);
  printf("current heap start: %p\n", p);

  
  // printf("%lu\n", lisp_str->tag);
  //printf("%lu\n", lisp_str->size);
  //  printf("%c\n", lisp_str->elements);

  //char *x = (char *) p;
  //x--;
  //printf("%c\n", *x);
  munmap(p, HEAP_SIZE);
}
