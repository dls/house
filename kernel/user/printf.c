/* Code for general_printf() */
/* Change extension to .c before compiling */

#define BITS_PER_BYTE           8

struct parameters
{
  int number_of_output_chars;
  short minimum_field_width;
  char options;
    #define MINUS_SIGN    1
    #define RIGHT_JUSTIFY 2
    #define ZERO_PAD      4
    #define CAPITAL_HEX   8
  short edited_string_length;
  short leading_zeros;
  int (*output_function)(void *, int);
  void *output_pointer;
};

static void output_and_count(struct parameters *p, int c)
{
  if (p->number_of_output_chars >= 0)
  {
    int n = (*p->output_function)(p->output_pointer, c);
    if (n>=0) p->number_of_output_chars++;
    else p->number_of_output_chars = n;
  }
}

static void output_field(struct parameters *p, char *s)
{
  short justification_length =
    p->minimum_field_width - p->leading_zeros - p->edited_string_length;
  if (p->options & MINUS_SIGN)
  {
    if (p->options & ZERO_PAD)
      output_and_count(p, '-');
    justification_length--;
  }
  if (p->options & RIGHT_JUSTIFY)
    while (--justification_length >= 0)
      output_and_count(p, p->options & ZERO_PAD ? '0' : ' ');
  if (p->options & MINUS_SIGN && !(p->options & ZERO_PAD))
    output_and_count(p, '-');
  while (--p->leading_zeros >= 0)
    output_and_count(p, '0');
  while (--p->edited_string_length >= 0)
    output_and_count(p, *s++);
  while (--justification_length >= 0)
    output_and_count(p, ' ');
}


int general_printf(int (*output_function)(void *, int), void *output_pointer,
  const char *control_string, const int *argument_pointer)
{
  struct parameters p;
  char control_char;
  p.number_of_output_chars = 0;
  p.output_function = output_function;
  p.output_pointer = output_pointer;
  control_char = *control_string++;
  while (control_char != '\0')
  {
    if (control_char == '%')
    {
      short precision = -1;
      short long_argument = 0;
      short base = 0;
      control_char = *control_string++;
      p.minimum_field_width = 0;
      p.leading_zeros = 0;
      p.options = RIGHT_JUSTIFY;
      if (control_char == '-')
      {
        p.options = 0;
        control_char = *control_string++;
      }
      if (control_char == '0')
      {
        p.options |= ZERO_PAD;
        control_char = *control_string++;
      }
      if (control_char == '*')
      {
        p.minimum_field_width = *argument_pointer++;
        control_char = *control_string++;
      }
      else
      {
        while ('0' <= control_char && control_char <= '9')
        {
          p.minimum_field_width =
            p.minimum_field_width * 10 + control_char - '0';
          control_char = *control_string++;
        }
      }
      if (control_char == '.')
      {
        control_char = *control_string++;
        if (control_char == '*')
        {
          precision = *argument_pointer++;
          control_char = *control_string++;
        }
        else
        {
          precision = 0;
          while ('0' <= control_char && control_char <= '9')
          {
            precision = precision * 10 + control_char - '0';
            control_char = *control_string++;
          }
        }
      }
      if (control_char == 'l')
      {
        long_argument = 1;
        control_char = *control_string++;
      }
      if (control_char == 'd')
        base = 10;
      else if (control_char == 'x')
        base = 16;
      else if (control_char == 'X')
      {
        base = 16;
        p.options |= CAPITAL_HEX;
      }
      else if (control_char == 'u')
        base = 10;
      else if (control_char == 'o')
        base = 8;
      else if (control_char == 'b')
        base = 2;
      else if (control_char == 'c')
      {
        base = -1;
        p.options &= ~ZERO_PAD;
      }
      else if (control_char == 's')
      {
        base = -2;
        p.options &= ~ZERO_PAD;
      }
      if (base == 0)  /* invalid conversion type */
      {
        if (control_char != '\0')
        {
          output_and_count(&p, control_char);
          control_char = *control_string++;
        }
      }
      else
      {
        if (base == -1)  /* conversion type c */
        {
          char c = *argument_pointer++;
          p.edited_string_length = 1;
          output_field(&p, &c);
        }
        else if (base == -2)  /* conversion type s */
        {
          char *string;
          p.edited_string_length = 0;
          string = * (char **) argument_pointer;
          argument_pointer += sizeof(char *) / sizeof(int);
          while (string[p.edited_string_length] != 0)
            p.edited_string_length++;
          if (precision >= 0 && p.edited_string_length > precision)
            p.edited_string_length = precision;
          output_field(&p, string);
        }
        else  /* conversion type d, b, o or x */
        {
          unsigned long x;
          char buffer[BITS_PER_BYTE * sizeof(unsigned long) + 1];
          p.edited_string_length = 0;
          if (long_argument) 
          {
            x = * (unsigned long *) argument_pointer;
            argument_pointer += sizeof(unsigned long) / sizeof(int);
          }
          else if (control_char == 'd')
            x = (long) *argument_pointer++;
          else
            x = (unsigned) *argument_pointer++;
          if (control_char == 'd' && (long) x < 0)
          {
            p.options |= MINUS_SIGN;
            x = - (long) x;
          }
          do 
          {
            int c;
            c = x % base + '0';
            if (c > '9')
            {
              if (p.options & CAPITAL_HEX)
                c += 'A'-'9'-1;
              else
                c += 'a'-'9'-1;
            }
            buffer[sizeof(buffer) - 1 - p.edited_string_length++] = c;
          }
          while ((x/=base) != 0);
          if (precision >= 0 && precision > p.edited_string_length)
            p.leading_zeros = precision - p.edited_string_length;
          output_field(&p, buffer + sizeof(buffer) - p.edited_string_length);
        }
        control_char = *control_string++;
      }
    }
    else
    {
      output_and_count(&p, control_char);
      control_char = *control_string++;
    }
  }
  return p.number_of_output_chars;
}

void putch(unsigned c) {
  __asm__ ("mov %0,%%ebx": : "rm"(c) : "%ebx");
  __asm__ ("mov $1,%%eax": : : "%eax");
  __asm__ ("int $0x80");
}

static int printf_helper(void *ptr, int c)
{
  putch(c);
  return 0;
}

int printf(char const * fmt, ...)
{
  general_printf(printf_helper, 0, fmt, (int *)(&fmt + 1));
}

