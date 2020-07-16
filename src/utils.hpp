#ifndef VCTRS_UTILS_HPP
#define VCTRS_UTILS_HPP


extern "C" {

// Defined in utils.c
void stop_internal(const char* fn, const char* fmt, ...);
void stop_unimplemented_type(const char* fn, SEXPTYPE type);

}

static inline
void stop_unknown(const char* fn) {
  stop_internal(fn, "Caught unknown exception.");
}


struct unimplemented_type_exception {
  const char* fn;
  SEXPTYPE type;
  unimplemented_type_exception(const char* fn_,
                               SEXPTYPE type_) :
      fn(fn_),
      type(type_)
  { }
};

#define RETHROW()                                       \
  catch (const unimplemented_type_exception& exc) {     \
    stop_unimplemented_type(exc.fn, exc.type);          \
  } catch (...) {                                       \
    stop_unknown("equal_fill");                         \
  }


#endif
