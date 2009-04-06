SRCS = e_acos.c e_asin.c e_cosh.c e_exp.c e_log.c e_pow.c e_rem_pio2.c e_sinh.c e_sqrt.c k_cos.c k_rem_pio2.c k_sin.c k_tan.c s_atan.c s_copysign.c s_cos.c s_expm1.c s_finite.c s_ldexp.c s_scalbn.c s_sin.c s_tan.c s_tanh.c w_acos.c w_asin.c w_cosh.c w_exp.c w_log.c w_pow.c w_sinh.c w_sqrt.c
OBJS = $(patsubst %.c,%.o,$(SRCS))

all: $(OBJS)
	ar rc ../libtiny_m.a *.o
	ranlib ../libtiny_m.a

%.o: %.c
	gcc -c -O3 -Dlint -D_IEEE_LIBM $< -o $@

clean:
	rm -rf *.o *~

