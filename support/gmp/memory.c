
typedef unsigned int size_t;
extern void* malloc(size_t n);
extern void free(void* p);
extern void* realloc(void* p, size_t n);

void *__gmp_default_allocate (size_t size) {
  void *ret = malloc (size);
  if (ret == 0)
    abort ();
  return ret;
}

void *__gmp_default_reallocate (void *oldptr, size_t old_size, size_t new_size) {
  void *ret = realloc (oldptr, new_size);
  if (ret == 0)
    abort ();
  return ret;
}

void __gmp_default_free (void *blk_ptr, size_t blk_size) {
  free (blk_ptr);
}

void *	(*__gmp_allocate_func) (size_t) = __gmp_default_allocate;
void *	(*__gmp_reallocate_func) (void *, size_t, size_t) = __gmp_default_reallocate;
void	(*__gmp_free_func) (void *, size_t) = __gmp_default_free;
