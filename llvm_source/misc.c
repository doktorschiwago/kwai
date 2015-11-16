#include <assert.h>

typedef union { double dval; int ival; } scalar_value_t;

extern SEXP R_ReplaceFunsTable;
extern SEXP R_SubsetSym;
extern SEXP R_SubassignSym;
extern SEXP R_Subset2Sym;
extern SEXP R_Subassign2Sym;
extern SEXP R_DollarGetsSymbol;
extern SEXP R_AddSym;
extern SEXP R_DivSym;
extern SEXP R_ExpSym;
extern SEXP R_SqrtSym;
extern SEXP R_ExptSym;
extern SEXP R_SubSym;
extern SEXP R_MulSym;
extern SEXP R_EqSym;
extern SEXP R_NeSym;
extern SEXP R_LtSym;
extern SEXP R_LeSym;
extern SEXP R_GtSym;
extern SEXP R_GeSym;
extern SEXP R_AndSym;
extern SEXP R_OrSym;
extern SEXP R_NotSym;
extern SEXP R_LogSym;

#define INTEGER_TO_REAL(x) ((x) == NA_INTEGER ? NA_REAL : (x))

static R_INLINE int bcStackScalarEx(SEXP s, scalar_value_t *v,
				    SEXP *pv)
{
    SEXP x = s;
    if (IS_SIMPLE_SCALAR(x, REALSXP)) {
	v->dval = REAL(x)[0];
	return REALSXP;
    }
    else if (IS_SIMPLE_SCALAR(x, INTSXP)) {
	v->ival = INTEGER(x)[0];
	return INTSXP;
    }
    else if (IS_SIMPLE_SCALAR(x, LGLSXP)) {
	v->ival = LOGICAL(x)[0];
	return LGLSXP;
    }
    else return 0;
}

static R_INLINE int bcStackScalarRealEx(SEXP s, scalar_value_t *px,
					SEXP *pv)
{
    int typex = bcStackScalarEx(s, px, pv);
    if (typex == INTSXP) {
	typex = REALSXP;
	px->dval = INTEGER_TO_REAL(px->ival);
	if (pv) *pv = NULL;
    }
    return typex;
}

static R_INLINE SEXP GETSTACK_PTR_TAG(SEXP s)
{
	return NULL;
	
}

double R_POW(double x, double y) {
	return R_pow(x, y);
}
