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
#include <dirent.h>

#define STATIC_SPACE_START 0x20000000
#define STATIC_SPACE_SIZE (30 * 1024 * 1024)

#define LISP_HEAP_START 0x200000000
#define STACK_SIZE (5 * 1024 * 1024)
#define LISP_HEAP_SIZE (100 * 1024 * 1024)

lispobj init_lisp(uintptr_t stack, uintptr_t heap);
lispobj run_lisp_test(uintptr_t stack, uintptr_t heap, uintptr_t code_address);
void print_lisp(lispobj obj);

int64_t read_long(FILE *fp);


uintptr_t allocation_start = LISP_HEAP_START - STACK_SIZE;
intptr_t stack_start = LISP_HEAP_START - 0x10; // 16 byte aligned
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

int is_cons(lispobj obj) {
  return (obj & MASK) == CONS_TAG ? 1 : 0;
}
    
int is_fun(lispobj obj) {
  return (obj & MASK) == FUNCTION_TAG ? 1 : 0;
}

int is_symbol(lispobj obj) {
  return (obj & MASK) == SYMBOL_TAG ? 1 : 0;
}




int is_simple_array(lispobj obj) {

  if(!is_pointer(obj)) {
    return 0;
  }

  lispobj *tag = (lispobj *) untag_pointer(obj);

  return (*tag & 0xFF) == SIMPLE_ARRAY_TAG ? 1 : 0;
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
  } else if (is_cons(obj)) {
    return CONS;
  } else if (is_fun(obj)) {
    return FUNCTION;
  } else if (is_symbol(obj)) {
    return SYMBOL;
  } else {
    return -1;
  }
}




void print_lisp_cons_cdr(lispobj obj) {

  lispobj _car = car(obj);
  lispobj _cdr = cdr(obj);

  print_lisp(_car);
  if (_cdr != LISP_NIL) {
    printf(" ");
  }

  enum base_lisp_type cdr_type = get_lisp_type(_cdr);

  if (_cdr == LISP_NIL) {
    printf(")");
  } else if (cdr_type == CONS) {
    print_lisp_cons_cdr(_cdr);
  } else {
    printf(" . ");
    print_lisp(_cdr);
    printf(")");
  }
  
}

void print_lisp_cons(lispobj obj) {

  printf("(");
  
  lispobj _car = car(obj);
  lispobj _cdr = cdr(obj);

  print_lisp(_car);
    if (_cdr != LISP_NIL) {
    printf(" ");
  }

  enum base_lisp_type cdr_type = get_lisp_type(_cdr);

  if (_cdr == LISP_NIL) {
    printf(")");
  } else if (cdr_type == CONS) {
    print_lisp_cons_cdr(_cdr);
  } else {
    printf(" . ");
    print_lisp(_cdr);
    printf(")");
  }
}

void print_lisp_string(lispobj obj) {
  
  struct array *ar = (struct array *) untag_pointer(obj);
  lispobj size = ar->size;
  int64_t array_size = untag_fixnum(size);

  lispobj *first = &ar->elements;

  printf("\"");

  for (long index = 0; index < array_size; index++) {
    print_lisp(*(first + index));
  }

  printf("\"");
}

void print_lisp_array(lispobj obj) {

   struct array *ar = (struct array *) untag_pointer(obj);
   lispobj size = ar->size;
   int64_t array_size = untag_fixnum(size);

   lispobj *first = &ar->elements;

   printf("#(");

   for (long index = 0; index < array_size; index++) {
     print_lisp(*(first + index));
     if (index + 1 < array_size)
       printf(" ");
   }

   printf(")");
}


void print_lisp_pointer(lispobj obj) {
  if (is_simple_array(obj)) {
    print_lisp_array(obj);
  } else if (is_string(obj)) {
    print_lisp_string(obj);
  } else {
    printf("UNKNOWN POINTER");
  }
}

void print_lisp_symbol(lispobj obj) {
  printf("#:");
  print_lisp_string(symbol_name(obj));
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
    case CONS :
      print_lisp_cons(obj);
      break;
    case FUNCTION :
      printf("FUNCTION\n");
      break;
    case POINTER :
      print_lisp_pointer(obj);
      break;
    case SYMBOL_TAG :
      print_lisp_symbol(obj);
      break;
    default :
      printf("UNKNOWN PRINT: %d", type);
      break;
    }
  }
  fflush(stdout);
}

void lisp_error(lispobj error_msg) {
  printf("LISP ERROR: ");
  print_lisp_string(error_msg);
  exit(1);
}


void *allocate_static_memory() {
  return mmap((void *) static_memory_start,
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


void load_code(char *code, int size) {

  void *current_heap = (void *) *heap_header;
  memcpy(current_heap, code, size);

  int word_aligned_size = size;
  // we are already aligning code
  //  int word_aligned_size = ((size / WORD_SIZE) + (size % WORD_SIZE > 0 ? 1 : 0));
  // word_aligned_size *= WORD_SIZE;
  
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
  //  printf("STACK START: %p\n", stack_start);
  printf("STATIC MEMORY: %p\n", static_memory);

  //set NIL car and cdr to start of static memory
  lispobj *lp = static_memory;
  *lp = LISP_NIL;
  lp++;
  *lp = LISP_NIL;
  lp++;
  
  // save stack memory pointer
  static_start = (void *) lp;

  heap_header = (lispobj *) current_heap;

  // increase heap for header size
  *heap_header = (lispobj) (heap_header + 2);
  *(heap_header+1) = (lispobj) heap_end;

  printf("HEAP_HEADER_START: %p\n", (void *) (*heap_header));
  printf("HEAP_HEADER_END: %p\n", (void *) (*(heap_header+1)));
  
  // print_lisp(init_lisp(stack_start, (uintptr_t) heap_header));
}

void load_core() {
}

struct lisp_code load_test_code(char *file) {
  
  struct lisp_code lcode;
  struct stat info;
  
  stat(file, &info);
  
  char *code = malloc((info.st_size - sizeof(int64_t)) * sizeof(char));
  
  printf("Code File Size: %lld\n", info.st_size);
  
  FILE *fp = fopen(file, "rb");
  int64_t start_address = read_long(fp);
  fread(code, info.st_size - sizeof(int64_t), 1, fp);
  fclose(fp);

  lcode.start_address = start_address;
  lcode.code = code;
  lcode.code_size = info.st_size - sizeof(int64_t);

  printf("Start address: %lli\n", lcode.start_address);
  return lcode;
}

lispobj run_test(char *test_file) {
  
  struct lisp_code lcode = load_test_code(test_file);
  uintptr_t code_address = (uintptr_t) *heap_header;
  
  load_code(lcode.code, lcode.code_size);
  free(lcode.code);

  return(run_lisp_test(stack_start, (uintptr_t) heap_header, lcode.start_address));
  //  run_lisp_test(stack_start, (uintptr_t) heap_header, lcode.start_address);
}

int main(int argc, char *argv[]) {

  init_runtime();
  
  load_core();

  if (argc == 2) {
    
    lispobj result = run_test(argv[1]);
    printf("\nGOT FROM LISP: ");
    print_lisp(result);
    printf("\n");

  } else if (argc == 3) {
    
    lispobj expected;
    
    if (strcmp(argv[2], "T") == 0) {
      expected = LISP_T;
    } else {
      expected = LISP_NIL;
    }

    lispobj result = run_test(argv[1]);

    if (result != expected) {

      printf("\n== ERROR in %s ==\n", argv[1]);
      printf("\nGOT FROM LISP: ");
      print_lisp(result);
      printf("\n");
      
      return(1);
    }
    
  } else {
    
    printf("Wrong number of arguments !!!");
    return(1);

  }

      
  destroy_runtime();

  return(0);
  
}

int from_lisp_test() {
  printf("this is test\n");
  return(80);
}














