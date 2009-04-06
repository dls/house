
void __gmp_assert_header (const char *filename, int linenum)
{
}

void __gmp_assert_fail (const char *filename, int linenum, const char *expr)
{
  __gmp_assert_header (filename, linenum);
  abort();
}
