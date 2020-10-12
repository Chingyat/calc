#ifndef CALC_H
#define CALC_H

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

/**
 * Parse and evaluate an expression
 *
 * @param expr a nul-terminated string containing the expression
 * @param result pointer to a floating pointer number where the results will be stored after a successful call to this function
 * @return true if success; otherwise false
 */
bool eval_expr_c(const char *expr,
                 double *result);

#ifdef __cplusplus
} // extern "C"
#endif // __cplusplus

#endif /* CALC_H */
