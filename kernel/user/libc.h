typedef unsigned int size_t;

int abs(int j);
int isdigit(int c);

void *memcpy(void *d, const void *s, size_t n);
void *memmove(void* d, void* s, size_t n);
void *memset(void *d, int c, size_t n); 

int strcmp(const char *s1, const char *s2);
char *strcpy(char *dest, const char *src);
char *strncpy(char *dest, const char *src, size_t n);
size_t strlen(const char *s);
size_t strnlen(const char *s, size_t maxlen);
char *strrchr (const char *s, int c);
double atof(char *s);


void *malloc(int);
