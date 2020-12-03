#include <stdint.h>
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
#define POINTER_TAG 0x1 // #b001
#define CONS_TAG 0x2 // #/* b010 */
#define FUNCTION_TAG 0x3 // #b011
#define CHAR_TAG 0x4 // #b100
#define SYMBOL_TAG 0x5 //#b101
#define SINGLE_FLOAT_TAG 0x6 //#b110

#define EXTENDED_TAG_SIMPLE_ARRAY 0xD1
#define EXTENDED_TAG_STRING 0xD9
#define EXPTENDET_TAG_STRUCT 0xF0

// tag 0x7 is free

enum base_lisp_type {FIXNUM, CHAR, CONS, FUNCTION, POINTER, SYMBOL};
enum pointer_lisp_type {STRING, UNKNOWN};

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
  lispobj type;
  lispobj etype;
  lispobj elements;
};

struct structure {
  lispobj tag;
  lispobj layoyt;
  lispobj struct_type;
  lispobj elements;
};

struct symbol {
  lispobj name;
  lispobj function;
  lispobj value;
  lispobj plist;
  lispobj package;
};

struct lisp_code {
  int64_t start_address;
  char *code;
  long code_size;
};

struct fixups {
  uint64_t size;
  struct eval_fixup *efixups;
};

struct eval_fixup {
  uintptr_t fun;
  uintptr_t fixup;
};
 
struct cons *allocate_cons(void **heap, lispobj car, lispobj cdr);
lispobj tag_cons(struct cons *cons);
struct cons *untag_cons(lispobj cons);
lispobj car(lispobj lisp_cons);
lispobj cdr(lispobj lisp_cons);

lispobj symbol_name(lispobj symbol);
lispobj symbol_value(lispobj symbol);
lispobj symbol_function(lispobj symbol);
lispobj symbol_name(lispobj symbol);
lispobj symbol_package(lispobj symbol);

struct array *allocate_string(void **heap, char *cstring);
lispobj tag_array(struct array *array);

lispobj tag_fixnum(int64_t num);
int64_t untag_fixnum(lispobj obj);

lispobj tag_char(char c);
char untag_char(lispobj obj);

lispobj tag_pointer(uintptr_t pointer);
lispobj untag_pointer(lispobj obj);

int string_equal(struct array *s1, struct array *s2);
int is_symbol_interned(lispobj symbol, lispobj symbols_list);

char * c_string(lispobj string);

lispobj lisp_open(lispobj file);
lispobj lisp_close(lispobj stream);
lispobj lisp_read_char(lispobj fd);
void c_print_lisp_string(lispobj s);

lispobj lisp_test_negative();

void lisp_error(lispobj message);
