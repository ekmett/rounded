/* --------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1992-2004
 *
 * mkMpfrDerivedConstants.c
 *
 * Basically this is a C program that extracts information from the C
 * declarations in the header files (primarily struct field offsets)
 * and generates a header file that can be #included into non-C source
 * containing this information.
 *
 * ------------------------------------------------------------------------*/

#include <stdio.h>
#include <gmp.h>
#include <mpfr.h>

#define str(a,b) #a "_" #b

#define OFFSET(s_type, field) ((size_t)&(((s_type*)0)->field))

/* struct_size(TYPE)
 *
 */
#define def_size(str, size) \
    printf("#define SIZEOF_" str " %lu\n", (unsigned long)size);

#define struct_size(s_type) \
    def_size(#s_type, sizeof(s_type));

/* struct_field(TYPE, FIELD)
 *
 */
#define def_offset(str, offset) \
    printf("#define OFFSET_" str " %d\n", (int)(offset));

#define field_offset_(str, s_type, field) \
    def_offset(str, OFFSET(s_type,field));

#define field_offset(s_type, field) \
    field_offset_(str(s_type,field),s_type,field);

#define field_type_(str, s_type, field) \
    printf("#define REP_" str " b"); \
    printf("%lu\n", (unsigned long)sizeof (__typeof__(((((s_type*)0)->field)))) * 8);

#define field_type(s_type, field) \
    field_type_(str(s_type,field),s_type,field);

/* An access macro for use in C-- sources. */
#define struct_field_macro(str) \
    printf("#define " str "(__ptr__)  REP_" str "[__ptr__+OFFSET_" str "]\n");

/* Outputs the byte offset and MachRep for a field */
#define struct_field(s_type, field)   \
    field_offset(s_type, field);    \
    field_type(s_type, field);      \
    struct_field_macro(str(s_type,field))

#define subfield_offset(s_type, field, subfield) \
    field_offset_(#s_type "_" #field "_" #subfield,s_type,field.subfield);

#define subfield_type(s_type, field, subfield) \
    field_type_(#s_type "_" #field "_" #subfield,s_type,field.subfield);

#define struct_subfield(s_type, field, subfield)   \
    subfield_offset(s_type, field, subfield);    \
    subfield_type(s_type, field, subfield);      \
    struct_field_macro(#s_type "_" #field "_" #subfield)

typedef __mpfr_struct MPFR;
typedef union { mp_size_t s; mp_limb_t l; } MPFR_SIZE_LIMB;

int
main(int argc, char *argv[])
{
    printf("/* This file is created automatically.  Do not edit by hand.*/\n\n");

    struct_size(MP_INT);
    struct_field(MP_INT,_mp_alloc);
    struct_field(MP_INT,_mp_size);
    struct_field(MP_INT,_mp_d);
    printf("\n");

    struct_size(MP_RAT);
    struct_subfield(MP_RAT,_mp_num,_mp_alloc);
    struct_subfield(MP_RAT,_mp_num,_mp_size);
    struct_subfield(MP_RAT,_mp_num,_mp_d);
    struct_subfield(MP_RAT,_mp_den,_mp_alloc);
    struct_subfield(MP_RAT,_mp_den,_mp_size);
    struct_subfield(MP_RAT,_mp_den,_mp_d);
    printf("\n");

    struct_size(MPFR);
    struct_field(MPFR,_mpfr_prec);
    struct_field(MPFR,_mpfr_sign);
    struct_field(MPFR,_mpfr_exp);
    struct_field(MPFR,_mpfr_d);
    struct_size(MPFR_SIZE_LIMB);
    printf("#define PREC_SHIFT %d\n", (int)(sizeof(mpfr_prec_t)*8-1));
    printf("#define MPFR_MANGLE_PTR(x) (x + SIZEOF_MPFR_SIZE_LIMB)\n");
    printf("#define MPFR_UNMANGLE_PTR(x) (x - SIZEOF_MPFR_SIZE_LIMB)\n");
    return 0;
}
