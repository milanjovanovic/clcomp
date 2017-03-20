#include <stdio.h>
#include <stdlib.h>
#include <string.h>


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

void read_nm(char *file) {
  FILE *nm = fopen(file, "r");
  char *line = malloc(255);
  while (fgets(line, 255, nm) != NULL)  {
    char *name = get_part(line, ' ', 0);
    // skip one _
    name++;
    char *address = get_part(line, ' ', 2);
    // FIXME, create conses
    printf("name: %s, address: %s\n", name, address);
  }
   free(line);
   fclose(nm);
}
