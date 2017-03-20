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


#define STACK_SIZE (2 * 1024 * 1024)
#define STATIC_HEAP_SIZE (30 * 1024 * 1024)
#define LISP_HEAP_SIZE (100 * 1024 * 1024)
#define LISP_HEAP_START 0x200000000

lispobj entry(uintptr_t stack, uintptr_t heap);

/*
  high address
---------------- 0x200000000
   lisp heap
----------------
   static heap
---------------- 
    stack
----------------
 low addresses
*/

uintptr_t allocation_start = LISP_HEAP_START - (STACK_SIZE + STATIC_HEAP_SIZE);
uintptr_t stack_start = LISP_HEAP_START - (STATIC_HEAP_SIZE + WORD_SIZE);
uintptr_t static_heap_start = LISP_HEAP_START - STATIC_HEAP_SIZE;

size_t memory_size = LISP_HEAP_SIZE + STACK_SIZE  + STATIC_HEAP_SIZE;

void *current_heap;


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
      // FIXME, this don't work for negative fixnums
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

void *allocate_heap() {
  return mmap((void *) allocation_start,
	      memory_size,
	      PROT_READ | PROT_WRITE | PROT_EXEC,
	      MAP_PRIVATE | MAP_ANON | MAP_FIXED, -1, 0);
}
  
void copy_code(void *code, int size) {
  //  memcpy((void *) heap, code, size);
}

int main() {

  
  current_heap = allocate_heap();
  
  printf("memory start address: %p\n", current_heap);
  printf("static heap start address: %p\n", static_heap_start);
  printf("stack start: %p\n", stack_start);

  current_heap += STACK_SIZE + STATIC_HEAP_SIZE;
  
  printf("lisp heap start address: %p\n", current_heap);
  
  // set NIL car and cdr
  lispobj *lp = current_heap;
  *lp = LISP_NIL;
  lp++;
  *lp = LISP_NIL;
  lp++;
  current_heap = (void *) lp;

  printf("heap start address: %p\n", current_heap);

  char *cstr = "abcd";
  struct array *s1 = allocate_string(&current_heap, cstr);

  printf("heap start address: %p\n", current_heap);
  printf("%d\n", is_string(tag_array(s1)));

  //print_lisp(entry(stack, heap));
  
  munmap((void *) allocation_start, memory_size);
}
