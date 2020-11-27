#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "hash.h"


char *get_part(char *str, char delim, int index) {
  char *start = str;
  char *current = start;
  int i = 0;
  int add = 0;
  while((current = strchr(start + add, delim)) != NULL) {
    if(i == index) {
      return strndup(start, current - start) + add;
    } else {
      i++;
      add = 1;
      start = current;
    }
  }
  return NULL;
}

void fill_map(char *file, struct hashmap *map) {
  FILE *nm = fopen(file, "r");
  char *line = malloc(255);
  while (fgets(line, 255, nm) != NULL)  {
    char *name = get_part(line, ' ', 0);
    // skip one _
    name++;
    char *address = get_part(line, ' ', 2);
    uintptr_t value = strtoull(address, NULL, 10);
    map_set(map, name, value);
    //printf("name: %s, address: %lu\n", name, value);
  }
  free(line);
  fclose(nm);
}

int64_t read_long(FILE *fp) {
  
  int64_t address = 0;
  unsigned char buffer[8];
  fread(buffer, 1, 8, fp);

  address |= (int64_t) buffer[0] << 56;
  address |= (int64_t) buffer[1] << 48;
  address |= (int64_t) buffer[2] << 40;
  address |= (int64_t) buffer[3] << 32;
  address |= (int64_t) buffer[4] << 24;
  address |= (int64_t) buffer[5] << 16;
  address |= (int64_t) buffer[6] << 8;
  address |= (int64_t) buffer[7];

  return address;
  
}

struct hashmap *create_nm_hashmap(char *file_name) {
  struct hashmap *map = map_allocate(100);
  fill_map(file_name, map);
  return map;
}
