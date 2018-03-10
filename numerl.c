#include "erl_nif.h"
#include <stdlib.h>

typedef struct Matrix {
    double *ptr;
    int rows;
    int cols;
    int start;
    int diff_x;
    int diff_y;
} matrix;

void init_matrix(matrix *matr, int rows, int cols) {
    matr->ptr = malloc((cols > 0 ? rows * cols : rows) * sizeof(double));
    matr->rows = rows;
    matr->cols = cols;
    matr->start = matr->diff_x = matr->diff_y = -1;
}

ERL_NIF_TERM matrix_to_tuple(ErlNifEnv *env, const matrix *matr) {
    ERL_NIF_TERM ptr_term = enif_make_int64(env, matr->ptr);
    ERL_NIF_TERM rows_term = enif_make_int(env, matr->rows);
    ERL_NIF_TERM cols_term = enif_make_int(env, matr->cols);
    ERL_NIF_TERM start_term = enif_make_int(env, matr->start);
    ERL_NIF_TERM diff_x_term = enif_make_int(env, matr->diff_x);
    ERL_NIF_TERM diff_y_term = enif_make_int(env, matr->diff_y);
    return enif_make_tuple6(env, ptr_term, rows_term, cols_term, start_term, diff_x_term, diff_y_term);
}

void list_to_arr(ErlNifEnv *env, ERL_NIF_TERM list, double *arr, int len) {
    for (int i = 0; i < len; ++i) {
        ERL_NIF_TERM head, tail;
        enif_get_list_cell(env, list, &head, &tail);

        double val;
        enif_get_double(env, head, &val);

        arr[i] = val;
        list = tail;
    }
}

static ERL_NIF_TERM from_list_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned int rows;
    enif_get_list_length(env, argv[0], &rows);

    ERL_NIF_TERM list = argv[0];
    ERL_NIF_TERM head, tail;
    enif_get_list_cell(env, list, &head, &tail);

    if (enif_is_number(env, head)) {
        matrix vector;
        init_matrix(&vector, rows, 0);
        list_to_arr(env, list, vector.ptr, rows);
        return matrix_to_tuple(env, &vector);
    } else {
        unsigned int cols;
        enif_get_list_length(env, head, &cols);

        matrix matr;
        init_matrix(&matr, rows, cols);

        for (int i = 0; i < rows; ++i) {
            ERL_NIF_TERM head, tail;
            enif_get_list_cell(env, list, &head, &tail);

            list_to_arr(env, head, matr.ptr + i * cols, cols);
            list = tail;
        }

        return matrix_to_tuple(env, &matr);
    }
}

void tuple_to_matrix(ErlNifEnv *env, const ERL_NIF_TERM obj, matrix *matr) {
    const ERL_NIF_TERM *tuple;
    int tuple_len;
    enif_get_tuple(env, obj, &tuple_len, &tuple);

    enif_get_int64(env, tuple[0], &matr->ptr);
    enif_get_int(env, tuple[1], &matr->rows);
    enif_get_int(env, tuple[2], &matr->cols);
    enif_get_int(env, tuple[3], &matr->start);
    enif_get_int(env, tuple[4], &matr->diff_x);
    enif_get_int(env, tuple[5], &matr->diff_y);
}

ERL_NIF_TERM arr_to_list(ErlNifEnv *env, double *arr, int len) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (int i = len - 1; i >= 0; --i) {
        ERL_NIF_TERM head = enif_make_double(env, arr[i]);
        ERL_NIF_TERM tail = list;
        list = enif_make_list_cell(env, head, tail);
    }
    return list;
}

static ERL_NIF_TERM to_list_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    matrix matr;
    tuple_to_matrix(env, argv[0], &matr);

    if (matr.cols == 0) {
        return arr_to_list(env, matr.ptr, matr.rows);
    } else if (matr.start != -1) {
        int size;
        if (matr.diff_y == 1) {
            size = matr.rows > matr.cols ? matr.cols : matr.rows;
        } else {
            size = matr.rows;
        }
        double *arr = malloc(size * sizeof(double));
        for (int x = 0, y = matr.start, i = 0;
             x < matr.rows && y < matr.cols; x += matr.diff_x, y += matr.diff_y, ++i) {
            int offset = x * matr.cols + y;
            arr[i] = matr.ptr[offset];
        }
        ERL_NIF_TERM list = arr_to_list(env, arr, size);
        free(arr);
        return list;
    } else {
        ERL_NIF_TERM list = enif_make_list(env, 0);
        for (int i = matr.rows - 1; i >= 0; --i) {
            ERL_NIF_TERM head = arr_to_list(env, matr.ptr + i * matr.cols, matr.cols);
            ERL_NIF_TERM tail = list;
            list = enif_make_list_cell(env, head, tail);
        }
        return list;
    }
}

int get_vector_len(matrix *vector) {
    if (vector->diff_y == 1) {
        return vector->rows > vector->cols ? vector->cols : vector->rows;
    }
    return vector->rows;
}

void iter_vector(matrix *vector, double *result) {
    for (int x = 0, y = vector->start, i = 0;
         x < vector->rows && y < vector->cols; x += vector->diff_x, y += vector->diff_y, ++i) {
        int offset = x * vector->cols + y;
        result[i] = vector->ptr[offset];
    }
}

void sum(const double *first, const double *second, int len, double *result) {
    for (int i = 0; i < len; ++i) {
        result[i] = first[i] + second[i];
    }
}

static ERL_NIF_TERM sum_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    matrix first;
    matrix second;

    tuple_to_matrix(env, argv[0], &first);
    tuple_to_matrix(env, argv[1], &second);

    matrix result;
    int len;

    if (first.start == -1 && second.start == -1) {
        init_matrix(&result, first.rows, first.cols);
        len = first.cols > 0 ? first.rows * first.cols : first.rows;
    } else {
        if (first.start != -1) {
            len = get_vector_len(&first);
            double *new_ptr = malloc(sizeof(double) * len);
            iter_vector(&first, new_ptr);
            first.ptr = new_ptr;
        }
        if (second.start != -1) {
            len = get_vector_len(&second);
            double *new_ptr = malloc(sizeof(double) * len);
            iter_vector(&second, new_ptr);
            second.ptr = new_ptr;
        }
        init_matrix(&result, len, 0);
    }

    sum(first.ptr, second.ptr, len, result.ptr);
    return matrix_to_tuple(env, &result);
}

void mul(const double *arr, int len, double scalar, double *result) {
    for (int i = 0; i < len; ++i) {
        result[i] = arr[i] * scalar;
    }
}

static ERL_NIF_TERM mul_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (enif_is_number(env, argv[1])) {
        matrix matr;
        double scalar;

        tuple_to_matrix(env, argv[0], &matr);
        enif_get_double(env, argv[1], &scalar);

        if (matr.start != -1) {
            int len = get_vector_len(&matr);
            matrix result;

            init_matrix(&result, len, 0);
            iter_vector(&matr, result.ptr);
            mul(result.ptr, len, scalar, result.ptr);

            return matrix_to_tuple(env, &result);
        } else {
            matrix result;
            init_matrix(&result, matr.rows, matr.cols);

            int len = matr.cols > 0 ? matr.rows * matr.cols : matr.rows;
            mul(matr.ptr, len, scalar, result.ptr);

            return matrix_to_tuple(env, &result);
        }
    } else {
        matrix matr;
        matrix vector;
        tuple_to_matrix(env, argv[0], &matr);
        tuple_to_matrix(env, argv[1], &vector);

        matrix result;
        init_matrix(&result, matr.rows, 0);

        if (vector.start != -1) {
            double *new_ptr = malloc(sizeof(double) * matr.rows);
            iter_vector(&vector, new_ptr);
            vector.ptr = new_ptr;
        }

        for (int i = 0; i < matr.rows; ++i) {
            double acc = 0;
            for (int j = 0; j < matr.cols; ++j) {
                acc += matr.ptr[i * matr.cols + j] * vector.ptr[j];
            }
            result.ptr[i] = acc;
        }

        return matrix_to_tuple(env, &result);
    }
}

static ERL_NIF_TERM get_row_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    matrix matr;
    tuple_to_matrix(env, argv[0], &matr);

    unsigned int index;
    enif_get_int(env, argv[1], &index);

    matrix result;
    result.ptr = matr.ptr + index * matr.cols;
    result.rows = matr.cols;
    result.cols = 0;
    result.start = result.diff_x = result.diff_y = -1;

    return matrix_to_tuple(env, &result);
}

static ERL_NIF_TERM get_col_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    matrix matr;
    tuple_to_matrix(env, argv[0], &matr);

    unsigned int index;
    enif_get_int(env, argv[1], &index);

    matrix result;
    result.ptr = matr.ptr;
    result.rows = matr.rows;
    result.cols = matr.cols;
    result.start = index;
    result.diff_x = 1;
    result.diff_y = 0;

    return matrix_to_tuple(env, &result);
}

static ERL_NIF_TERM get_diag_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    matrix matr;
    tuple_to_matrix(env, argv[0], &matr);

    matrix result;
    result.ptr = matr.ptr;
    result.rows = matr.rows;
    result.cols = matr.cols;
    result.start = 0;
    result.diff_x = result.diff_y = 1;

    return matrix_to_tuple(env, &result);
}

static ErlNifFunc nif_funcs[] = {
        {"from_list", 1, from_list_nif},
        {"to_list",   1, to_list_nif},
        {"sum",       2, sum_nif},
        {"mul",       2, mul_nif},
        {"get_row",   2, get_row_nif},
        {"get_col",   2, get_col_nif},
        {"get_diag",  1, get_diag_nif},
};

ERL_NIF_INIT(numerl, nif_funcs, NULL, NULL, NULL, NULL)
