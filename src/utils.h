#ifndef VCTRS_UTILS_H
#define VCTRS_UTILS_H


#define SWAP(T, x, y) do {                      \
    T tmp = x;                                  \
    x = y;                                      \
    y = tmp;                                    \
  } while (0)


bool is_bool(SEXP x);

SEXP vctrs_dispatch_n(SEXP fn_sym, SEXP fn,
                      SEXP* syms, SEXP* args);
SEXP vctrs_dispatch1(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x);
SEXP vctrs_dispatch2(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x,
                     SEXP y_sym, SEXP y);
SEXP vctrs_dispatch3(SEXP fn_sym, SEXP fn,
                     SEXP x_sym, SEXP x,
                     SEXP y_sym, SEXP y,
                     SEXP z_sym, SEXP z);

SEXP df_map(SEXP df, SEXP (*fn)(SEXP));
SEXP with_proxy(SEXP x, SEXP (*rec)(SEXP, bool), SEXP i);
bool is_bare_tibble(SEXP x);

struct vctrs_arg args_empty;

void never_reached(const char* fn) __attribute__((noreturn));

enum vctrs_type2 vec_typeof2_impl(enum vctrs_type type_x, enum vctrs_type type_y, int* left);

bool is_compact_rownames(SEXP x);
R_len_t compact_rownames_length(SEXP x);
void init_data_frame(SEXP x, R_len_t n);
void init_tibble(SEXP x, R_len_t n);
bool is_native_df(SEXP x);

bool (*rlang_is_splice_box)(SEXP);
SEXP (*rlang_unbox)(SEXP);
SEXP (*rlang_env_dots_values)(SEXP);
SEXP (*rlang_env_dots_list)(SEXP);

R_len_t r_lgl_sum(SEXP lgl, bool na_true);
SEXP r_lgl_which(SEXP x, bool na_true);

void r_lgl_fill(SEXP x, int value);
void r_int_fill(SEXP x, int value);

void r_int_fill_seq(SEXP x, int start);

bool r_int_any_na(SEXP x);

SEXP r_new_environment(SEXP parent, R_len_t size);

SEXP r_protect(SEXP x);
bool r_is_true(SEXP x);
bool r_is_string(SEXP x);
SEXP r_peek_option(const char* option);
SEXP r_names(SEXP x);

SEXP r_pairlist(SEXP* tags, SEXP* cars);
SEXP r_call(SEXP fn, SEXP* tags, SEXP* cars);

SEXP r_names(SEXP x);
bool r_has_name_at(SEXP names, R_len_t i);

static inline const char* r_chr_get_c_string(SEXP chr, R_len_t i) {
  return CHAR(STRING_ELT(chr, i));
}

static inline void r__vec_get_check(SEXP x, R_len_t i, const char* fn) {
  if ((Rf_length(x) - 1) < i) {
    Rf_error("Internal error in `%s()`: Vector is too small", fn);
  }
}
static inline int r_lgl_get(SEXP x, R_len_t i) {
  r__vec_get_check(x, i, "r_lgl_get");
  return LOGICAL(x)[i];
}
static inline int r_int_get(SEXP x, R_len_t i) {
  r__vec_get_check(x, i, "r_int_get");
  return INTEGER(x)[i];
}
static inline double r_dbl_get(SEXP x, R_len_t i) {
  r__vec_get_check(x, i, "r_dbl_get");
  return REAL(x)[i];
}


extern SEXP vctrs_ns_env;
extern SEXP vctrs_shared_empty_str;

extern SEXP classes_data_frame;

extern SEXP syms_i;
extern SEXP syms_x;
extern SEXP syms_y;
extern SEXP syms_to;
extern SEXP syms_dots;
extern SEXP syms_bracket;
extern SEXP syms_x_arg;
extern SEXP syms_y_arg;

extern SEXP fns_bracket;
extern SEXP fns_quote;


#endif
