/*
 * The content of this file is in the public domain.
 */

typedef unsigned int size_t;

int abs(int j) {
  if (j < 0)
    return -j;
  else
    return j;
}

int isdigit(int c) {
  return (c >= '0' && c <= '9');
}

void *memcpy(void *d, const void *s, size_t n) {
  void *d0 = d;
  if (s != d)
    for (; 0 != n; --n)
      *(char*)d++ = *(char*)s++;
  return d0;
}

void *memmove(void* d, void* s, size_t n)
{
  char *cd = d;
  char *cs = s;
  if (s < d)
    for (cs += n, cd += n; 0 != n; --n)
      *--cd = *--cs;
  else if (s != d)
    for (; 0 != n; --n)
      *cd++ = *cs++;
  return d;
}

void *memset(void *d, int c, size_t n) {
  void *d0 = d;
  unsigned char b = (unsigned char)c;
  for (; 0 != n; --n)
    *(unsigned char*)d++ = b;
  return d0;
}

int strcmp(const char *s1, const char *s2) {
  for (; (*s1 != '\0') && (*s1 == *s2); ++s1, ++s2)
    ;
  return (*s1 - *s2);
}

char *strcpy(char *dest, const char *src)
{
  char * tmp = dest;
  for (; (*dest = *src) != 0; ++dest, ++src);
  return tmp;
}

char *strncpy(char *dest, const char *src, size_t n)
{
  if (n != 0) {
    char *d = dest;
    do {
      if ((*d++ = *src++) == 0) {
	while (--n != 0)
	  *d++ = 0;
	break;
      }
    } while (--n != 0);
  }
  return dest;
}

size_t strnlen(const char *s, size_t maxlen) {
  size_t len = 0;
  for (; s[len] != '\0' && len < maxlen; ++len)
    ;
  return len;
}

char *
strrchr (s, c)
     register const char *s;
     int c;
{
  char *rtnval = 0;

  do {
    if (*s == c)
      rtnval = (char*) s;
  } while (*s++);
  return (rtnval);
}

double atof_hop(char *s)
{
  double a = 0.0;
  int e = 0;
  int c;
  while ((c = *s++) != '\0' && isdigit(c)) {
    a = a*10.0 + (c - '0');
  }
  if (c == '.') {
    while ((c = *s++) != '\0' && isdigit(c)) {
      a = a*10.0 + (c - '0');
      e = e-1;
    }
  }
  if (c == 'e' || c == 'E') {
    int sign = 1;
    int i = 0;
    c = *s++;
    if (c == '+')
      c = *s++;
    else if (c == '-') {
      c = *s++;
      sign = -1;
    }
    while (isdigit(c)) {
      i = i*10 + (c - '0');
      c = *s++;
    }
    e += i*sign;
  }
  while (e > 0) {
    a *= 10.0;
    e--;
  }
  while (e < 0) {
    a *= 0.1;
    e++;
  }
  return a;
}
