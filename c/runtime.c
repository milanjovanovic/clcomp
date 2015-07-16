#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <dlfcn.h>


long heap_start_address = 0L;

long allocate_heap(int size) {
  void *addr = mmap(0, size, PROT_READ | PROT_WRITE | PROT_EXEC,
		    MAP_ANON | MAP_PRIVATE,
		    0, 0);
  return((long) addr);
}

int load_code(int size, char *code) {
  printf("%s", code);
  return(size);
}

int foo() {
  asm("mov $5, %eax");
}


/*
int main() {
}
*/

