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


#define STATIC_SPACE_START 0x20000000
#define STATIC_SPACE_SIZE (30 * 1024 * 1024)

#define LISP_HEAP_START 0x200000000
#define STACK_SIZE (2 * 1024 * 1024)
#define LISP_HEAP_SIZE (100 * 1024 * 1024)

lispobj init_lisp(uintptr_t stack, uintptr_t heap);

uintptr_t allocation_start = LISP_HEAP_START - STACK_SIZE;
uintptr_t stack_start = LISP_HEAP_START - WORD_SIZE;
uintptr_t heap_end = LISP_HEAP_START + LISP_HEAP_SIZE - WORD_SIZE;

uintptr_t static_memory_start = STATIC_SPACE_START;

size_t memory_size = LISP_HEAP_SIZE + STACK_SIZE;


void *static_start;
lispobj *heap_header;


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

int is_string(lispobj obj) {
  if(!is_pointer(obj)) {
    return 0;
  }

  lispobj *tag = (lispobj *) untag_pointer(obj);

  return (*tag & 0xFF) == CHAR_ARRAY_TAG ? 1 : 0;
   
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

  if(obj == LISP_NIL) {
    
    printf("NIL");
    
  } else if (obj == LISP_T) {
    
    printf("T");
    
  } else {

    enum base_lisp_type type = get_lisp_type(obj);
    
    switch(type) {
    
    case FIXNUM :
      printf("%lli", untag_fixnum(obj));
      break;
    case CHAR :
      printf("%c", untag_char(obj));
      break;
    case LIST :
      printf("(");
      print_lisp(car(obj));
      if(cdr(obj) == LISP_NIL) {
	printf(")");
      } else if(get_lisp_type(cdr(obj)) == LIST) {
	printf(" ");
	print_lisp(cdr(obj));
      } else {
	printf(" . ");
	print_lisp(cdr(obj));
      }
      printf(")");
      break;
    case FUNCTION :
      printf("FUNCTION\n");
      break;
    case POINTER :
      printf("POINTER\n");
    default :
      printf("UNKNOWN PRINT: %d", type);
      break;
    }
  }
}


void *allocate_static_memory() {
  return = mmap((void *) static_memory_start,
	      STATIC_SPACE_SIZE,
	      PROT_READ | PROT_WRITE | PROT_EXEC,
	      MAP_PRIVATE | MAP_ANON | MAP_FIXED, -1, 0);
}

void *allocate_heap() {
  return mmap((void *) allocation_start,
	      memory_size,
	      PROT_READ | PROT_WRITE | PROT_EXEC,
	      MAP_PRIVATE | MAP_ANON | MAP_FIXED, -1, 0);
}


void load_code(void *code, int size) {
  
  void *current_heap = (void *) *heap_header;
  memcpy(current_heap, code, size);
  
  int word_aligned_size = ((size / WORD_SIZE) + (size % WORD_SIZE > 0 ? 1 : 0));
  word_aligned_size *= WORD_SIZE;
  
  // save new start to heap_header
  *heap_header = (uintptr_t)( current_heap + word_aligned_size);

}

void destroy_runtime() {
  munmap((void *) allocation_start, memory_size);
  munmap((void *) STATIC_SPACE_START, STATIC_SPACE_SIZE);
}

void init_runtime() {

  void *static_memory = allocate_static_memory();
  void *current_heap = allocate_heap();

  current_heap += STACK_SIZE;

  printf("HEAP START: %p\n", current_heap);
  printf("STATIC MEMORY: %p\n", static_memory);

  //set NIL car and cdr to start of static memory
  lispobj *lp = static_memory;
  *lp = LISP_NIL;
  lp++;
  *lp = LISP_NIL;
  lp++;
  
  // save stack memory pointer
  static_start = (void *) lp;

  // save address as heap_header pointer
  heap_header = (lispobj *) current_heap;
  *heap_header = (uintptr_t) (heap_header + 2); // moving heap start
  *(heap_header + 1) = heap_end; // heap end

  print_lisp(init_lisp(stack_start, (uintptr_t) heap_header));
 

  destroy_runtime();
}

int main() {
  init_runtime();
}
