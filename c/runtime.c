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


#define STACK_SIZE 2097152 // 2 MB
#define HEAP_SIZE 52428800 // 50 MB
#define HEAP_START 0x20000000
#define STACK (HEAP_START + (2 * 1024 * 1024))

#define WORDSIZE 0x4
#define ALLOC_WORDS 0x1 // we are allocating on 1 word/8 bytes boundaries
#define TAG_SIZE 0x3
#define MASK 0x7 

#define FIXNUM_TAG 0x0 // #b000
#define CONS_TAG 0x2 // #b010
#define FUNCTION_TAG 0x3 // #b011
#define CHAR_TAG 0x4 // #b100
#define POINTER_TAG 0x7 // #b111

// 0x1  free
// 0x5  free 
// 0x6  free

#define CHAR_SHIFT 0x8

typedef uintptr_t lispobj;

struct cons {
  lispobj car;
  lispobj cdr;
};


lispobj entry(uintptr_t stack, uintptr_t heap);

enum base_lisp_type {FIXNUM, CHAR, CONS, FUNCTION, POINTER}; 

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

int is_cons(lispobj obj) {
  return (obj & MASK) == CONS_TAG ? 1 : 0;
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
  } else if (is_cons(obj)) {
    return CONS;
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
  case CONS :
    printf("CONS\n");
    break;
  case FUNCTION :
    printf("FUNCTION\n");
    break;
  default :
    printf("UNKNOWN PRINT: %d", type);
    break;
  }
}

void *allocate_heap(int size) {
  void *addr = mmap(0, size, PROT_READ | PROT_WRITE | PROT_EXEC,
		    MAP_ANON | MAP_PRIVATE,
		    0, 0);
  return addr;
}
  
void copy_code(void *code, int size) {
  //  memcpy((void *) heap, code, size);
}

int main() {
  print_lisp(entry(stack, heap));
}
