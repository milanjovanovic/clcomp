#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include "hash.h"

long str_hash_code(char *str) {
  
  int h = 0;
  
  int size = strlen(str);
  if (strlen(str) > 0) {
    for (int i = 0; i < size; i++) {
      h = 31 * h + str[i];
    }
  }
  return abs(h);
}

struct hashmap *map_allocate(int size) {
  
  struct entry **entries = calloc(size, sizeof(struct entry **));
  if (entries == NULL)
    return NULL;


  struct hashmap *map = malloc(sizeof(struct hashmap));
  if (map == NULL)
    return NULL;
  
  map->entries = entries;
  map->size = size;
  map->filled = 0;

  return map;
  
}

int add_to_entries(struct entry **entries, int index, struct entry *new_entry) {

  struct entry *ee = entries[index];

  if (ee == NULL) {
    
    entries[index] = new_entry;

  } else {

    struct entry *current;
    struct entry *last;
     
    for(current = ee; current != NULL; current = current->next) {

      last = current;
       
      if (strlen(current->key) == strlen(new_entry->key) &&
	  strcmp(current->key, new_entry->key) == 0) {
	current->value = new_entry->value;
	free(new_entry->key);
	free(new_entry);
	return 0;
      }
    }
      
    last->next = new_entry;
  }

  return 1;
  
}

void rehash(struct hashmap *map) {

  int new_size = map->size * 2;
  
  int filled = 0;
  
  struct entry **new_entries = calloc(new_size * 2, sizeof(struct entry **));
  
  for (int i=0; i<map->size; i++) {

    struct entry *next;
    for (struct entry *e = map->entries[i]; e != NULL; e = next) {
      int index = str_hash_code(e->key) % new_size;
      next = e->next;
      e->next = NULL;
      filled += add_to_entries(new_entries, index, e);
    }
  }

  free(map->entries);
  map->entries = new_entries;
  map->size = new_size;
  map->filled = filled;
}

void maybe_rehash(struct hashmap *map) {
  float load = (float) map->filled / (float) map->size;
  if (load > 0.75f) {
    rehash(map);
  }
}

void map_set(struct hashmap *map, char *key, uintptr_t value) {

  maybe_rehash(map);
  int hash = str_hash_code(key);
  int index = hash % map->size;
  struct entry *new_entry = malloc(sizeof(struct entry));
  new_entry->key = key;
  new_entry->value = value;
  new_entry->next = NULL;
  map->filled += add_to_entries(map->entries, index, new_entry);

}

uintptr_t *map_get(struct hashmap *map, char *key) {
  
  int hash = str_hash_code(key);
  int index = hash % map->size;

  struct entry *entry = map->entries[index];

  if (entry == NULL)
    return NULL;

  do {
    
    if (strlen(entry->key) == strlen(key) &&
	strcmp(entry->key, key) == 0)
      return &(entry->value);
    
    entry = entry->next;

  } while (entry != NULL);


  return NULL;
}


void map_destroy(struct hashmap *map) {
  
  struct entry **entries = map->entries;
  
  for (int i=0; i < map->size; i++) {

    struct entry *next;
    for(struct entry *e = entries[i]; e != NULL; e = next) {
      next = e->next;
      free(e);
    }
  }

  free(entries);
  free(map);
}

int test() {

  struct hashmap *map = map_allocate(1000);

  for(int i = 0; i < 10000; i++) {
    int ssize = snprintf(NULL, 0, "%d", i);
    char *s = malloc(ssize + 1);
    snprintf(s, ssize+1, "%d", i);
    map_set(map, s, i);
  }

  for(int i = 0; i < 10000; i++) {
    int ssize = snprintf(NULL, 0, "%d", i);
    char *s = malloc(ssize + 1);
    snprintf(s, ssize+1, "%d", i);
    map_set(map, s, i+1);
  }

  map_set(map, "500", 600);
  map_set(map, "1000", 2000);

  for(int i = 0; i < 10000; i++) {
    
    int ssize = snprintf(NULL, 0, "%d", i);
    char *s = malloc(ssize + 1);
    snprintf(s, ssize+1, "%d", i);
    
    int result = 0;
    int r = *(map_get(map,s));
    
    if (r == i+1) {
      result = 1;
    }
    
    if(i == 500 || i == 1000) result = 1;

      
    assert(result);

  }

  free(map);

  return 0;
}
