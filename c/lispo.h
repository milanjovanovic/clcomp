#define LISP_NIL ((lispobj) 0x200000002) // LISP_NIL - LIST_TAG = HEAP
#define LISP_T ((lispobj) 0x20000000B))   // LISP_T - POINTER_TAG = HEAP + 8

#define WORD_SIZE 8

typedef uintptr_t lispobj;

#define TAG_SIZE 0x3
#define MASK 0x7 

#define FIXNUM_TAG 0x0 // #b000
#define LIST_TAG 0x2 // #b010
#define FUNCTION_TAG 0x3 // #b011
#define CHAR_TAG 0x4 // #b100
#define POINTER_TAG 0x7 // #b111

// 0x1  free
// 0x5  free 
// 0x6  free

enum base_lisp_type {FIXNUM, CHAR, LIST, FUNCTION, POINTER};

#define CHAR_SHIFT 0x8

struct cons {
  lispobj car;
  lispobj cdr;
};

#define CAR_OFFSET -2
#define CDR_OFFSET 6

struct array {
  lispobj tag;
  lispobj size;
  lispobj elements;
};

#define STRING_TAG 

struct symbol {
  lispobj tag;
  lispobj name;
  lispobj fun;
  lispobj value;
};

 
struct cons *cons(lispobj car, lispobj cdr);
struct cons *add(struct cons *head, struct cons *cons);
struct cons *allocate_lisp_cons(void **heap, lispobj car, lispobj cdr);
struct array *allocate_lisp_string(void **heap, char *cstring);
