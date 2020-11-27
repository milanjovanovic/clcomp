#include <stdint.h>

struct entry {
  struct entry *next;
  char *key;
  uintptr_t value;
};

struct hashmap {
  struct entry **entries;
  int size;
  int filled;
};


struct hashmap *map_allocate(int size);
void map_set(struct hashmap *map, char *key, uintptr_t value);
uintptr_t *map_get(struct hashmap *map, char *key);
void map_destroy(struct hashmap *map);
