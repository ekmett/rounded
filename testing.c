#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>
#include <mpfr.h>
#include "mpfr-impl.h"

void testing()
{
 mpz_t x;
 mpz_t y;
 mpz_t result;
 unsigned int i;
 mpfr_t s, t, u;

 mpz_init(x);
 mpz_init(y);
 mpz_init(result);

 mpz_set_str(x, "7612058254738945", 10);
 mpz_set_str(y, "9263591128439081", 10);

 mpz_mul(result, x, y);
 gmp_printf("\n    %Zd\n*\n    %Zd\n--------------------\n%Zd\n\n", x, y, result);

 /* free used memory */
 mpz_clear(x);
 mpz_clear(y);
 mpz_clear(result);

 printf("OHAI!!!! %d\n", __LINE__);

 printf("HACK testing %d at %p %p %p\n", __LINE__, &mpfr_allocate_func, &mpfr_reallocate_func, &mpfr_free_func);


 /* now try MPFR! */

 mpfr_init2 (t, 200);
 printf("OHAI!!!! %d\n", __LINE__);
 mpfr_set_d (t, 1.0, GMP_RNDD);
 printf("OHAI!!!! %d\n", __LINE__);
 mpfr_init2 (s, 200);
 printf("OHAI!!!! %d\n", __LINE__);
 mpfr_set_d (s, 1.0, GMP_RNDD);
 printf("OHAI!!!! %d\n", __LINE__);
 mpfr_init2 (u, 200);
 printf("OHAI!!!! %d\n", __LINE__);
 for (i = 1; i <= 100; i++)
   {
     mpfr_mul_ui (t, t, i, GMP_RNDU);
     mpfr_set_d (u, 1.0, GMP_RNDD);
     mpfr_div (u, u, t, GMP_RNDD);
     mpfr_add (s, s, u, GMP_RNDD);
   }
 printf ("Sum is ");
 mpfr_out_str (stdout, 10, 0, s, GMP_RNDD);
 printf("OHAI!!!! %d\n", __LINE__);
 putchar ('\n');
 printf("OHAI!!!! %d\n", __LINE__);
 mpfr_clear (s);
 printf("OHAI!!!! %d\n", __LINE__);
 mpfr_clear (t);
 printf("OHAI!!!! %d\n", __LINE__);
 mpfr_clear (u);
 printf("OHAI!!!! %d\n", __LINE__);
}
