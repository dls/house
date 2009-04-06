#ifndef HOUSE_UTIL_DOT_H
#define HOUSE_UTIL_DOT_H

#include <stdarg.h>

void kprintf(char const * fmt, ...);
void fprintf(void * dummy, char const * fmt, ...);
void vfprintf(void * dummy, char const * fmt, va_list ap);
int sprintf(char *s, char const *control, ...);
void fwrite(void const * ptr, size_t size, size_t nmemb, void * dummy);
ssize_t write(int fd, const void *buf, size_t count);
void fputc(int c, void * dummy);
void fputs(char const * str, void * dummy);
void fflush(void * dummy);
char * getenv(char const * dummy);
void * fopen(char const * path, char const * mode);
void c_print(char const * str);
void exit(int n);
void abort();
void perror(const char *s);

#endif
