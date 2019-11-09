// NIL car/cdr should be set to the same NIL value
#define LISP_NIL ((lispobj) 0x20000002)
// FIXME, T values has bad tag
// FIXME, it's not bad, it is good, T should have pointer low tag, but widetag is symbol
// FIXME, NIL doesn't have poenter low tag
#define LISP_T ((lispobj) 0x2000000F)  

#define WORD_SIZE 8

typedef uintptr_t lispobj;

#define TAG_SIZE 0x3
#define MASK 0x7
#define CLEAR_TAG_MASK 0xfffffffffffffff8

#define FIXNUM_TAG 0x0 // #b000
#define CONS_TAG 0x2 // #/* b010 */
#define FUNCTION_TAG 0x3 // #b011
#define CHAR_TAG 0x4 // #b100
#define SYMBOL_TAG 0x5 //#b101
#define POINTER_TAG 0x7 // #b111

// 0x6  free

enum base_lisp_type {FIXNUM, CHAR, CONS, FUNCTION, POINTER};
enum pointer_lisp_type {STRING, UNKNOWN};

#define CHAR_SHIFT 0x8

struct cons {
  lispobj car;
  lispobj cdr;
};

#define CAR_OFFSET -2
#define CDR_OFFSET 6

#define SIMPLE_ARRAY_TAG 0x1

struct array {
  lispobj tag;
  lispobj size;
  lispobj elements;
};

#define CHAR_ARRAY_TAG 0x2

struct symbol {
  lispobj name;
  lispobj fun;
  lispobj value;
  lispobj plist;
};

struct lisp_code {
  int64_t start_address;
  char *code;
  long code_size;
};
 
struct cons *allocate_cons(void **heap, lispobj car, lispobj cdr);
lispobj tag_cons(struct cons *cons);
struct cons *untag_cons(lispobj cons);
lispobj car(lispobj lisp_cons);
lispobj cdr(lispobj lisp_cons);


struct array *allocate_string(void **heap, char *cstring);
lispobj tag_array(struct array *array);

lispobj tag_fixnum(int64_t num);
int64_t untag_fixnum(lispobj obj);

lispobj tag_char(char c);
char untag_char(lispobj obj);

lispobj tag_pointer(uintptr_t pointer);
lispobj untag_pointer(lispobj obj);



