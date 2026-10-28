// Ximena Maria Hawith Garcia 251ADB044
// https://github.com/XimenaHaw/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: <every AI tool, website and book you used; details in comments where you used them>

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define NAME "XimenaMaria"
#define LASTNAME "HawithGarcia"
#define STUDENTID "251ADB044"


// Parser structure 
typedef struct {
    const char *text;
    size_t length; 
    size_t index;
    size_t line_end;
    size_t place_error;
    int failed;
} Parser;

// Structure to store the result
typedef struct {
    double value;
    int is_float;
} Value;

// Recurring used function to skip spaces
static void skip_spaces(Parser *p) {
    while (p->index < p->line_end &&
           (p->text[p->index] == ' ' || p->text[p->index] == '\t' ||
            p->text[p->index] == '\r')) {
        p->index++;
    }
}

static void set_error(Parser *p, size_t pos) {
    if (!p->failed) {
        p->failed = 1;
        p->place_error = pos;
    }
}

static Value parse_expr(Parser *p);
static Value parse_power(Parser *p);

static Value bad_value(void) {
    Value v = {0.0, 0};
    return v;
}

static Value parse_primary(Parser *p) {
    Value v = bad_value();
    skip_spaces(p);
    if (p->index >= p->line_end) {
        set_error(p, p->line_end < p->length ? p->line_end + 1 : p->length + 1);
        return v;
    }

    if (p->text[p->index] == '(') {
        p->index++;
        v = parse_expr(p);
        skip_spaces(p);
        if (!p->failed) {
            if (p->index >= p->line_end) {
                set_error(p, p->line_end < p->length ? p->line_end + 1 : p->length + 1);
            } else if (p->text[p->index] != ')') {
                set_error(p, p->index + 1);
            } else {
                p->index++;
            }
        }
        return v;
    }

    if (isdigit((unsigned char)p->text[p->index])) {
        size_t start = p->index;
        while (p->index < p->line_end && isdigit((unsigned char)p->text[p->index])) p->index++;
        if (p->index < p->line_end && p->text[p->index] == '.') {
            p->index++;
            if (p->index >= p->line_end || !isdigit((unsigned char)p->text[p->index])) {
                set_error(p, p->index);
                return v;
            }
            while (p->index < p->line_end && isdigit((unsigned char)p->text[p->index])) p->index++;
            v.is_float = 1;
        }
        size_t n = p->index - start;
        char *number = malloc(n + 1);
        if (number == NULL) {
            set_error(p, start + 1);
            return v;
        }
        memcpy(number, p->text + start, n);
        number[n] = '\0';
        errno = 0;
        v.value = strtod(number, NULL);
        free(number);
        if (errno == ERANGE || !isfinite(v.value)) set_error(p, start + 1);
        return v;
    }

    set_error(p, p->index + 1);
    return v;
}

// Exponents handling
static Value parse_power(Parser *p) {
    Value left = parse_primary(p);
    skip_spaces(p);
    if (p->failed) return left;
    if (p->index + 1 < p->line_end && p->text[p->index] == '*' && p->text[p->index + 1] == '*') {
        size_t op_pos = p->index;
        p->index += 2;
        skip_spaces(p);
        size_t exponent_pos = p->index;
        // The function is called to correctly handle exponents
        Value right = parse_power(p);
        if (p->failed) return left;
        if (!right.is_float && right.value < 0) {
            set_error(p, exponent_pos + 1);
            return left;
        }
        if (left.is_float || right.is_float) {
            left.value = pow(left.value, right.value);
            left.is_float = 1;
        } else {
            left.value = pow(left.value, right.value);
        }
        if (!isfinite(left.value)) set_error(p, op_pos + 1);
    }
    return left;
}

// This functions purpose is to do multiplication and division 
static Value parse_term(Parser *p) {
    Value left = parse_power(p);
    while (!p->failed) {
        skip_spaces(p);
        if (p->index >= p->line_end) break;
        char op = p->text[p->index];
        if (op != '*' && op != '/') break;
        if (op == '*' && p->index + 1 < p->line_end && p->text[p->index + 1] == '*') break;
        p->index++;
        skip_spaces(p);
        size_t right_pos = p->index;
        Value right = parse_power(p);
        if (p->failed) break;
        if (op == '/') {
            if (right.value == 0.0) { set_error(p, right_pos + 1); break; }
            if (left.is_float || right.is_float) {
                left.value /= right.value;
                left.is_float = 1;
            } else {
                left.value = (double)((long long)left.value / (long long)right.value);
            }
        } else {
            left.value *= right.value;
            left.is_float = left.is_float || right.is_float;
        }
        if (!isfinite(left.value)) set_error(p, right_pos + 1);
    }
    return left;
}

// This functions purpose is to do addition and substraction 
static Value parse_expr(Parser *p) {
    Value left = parse_term(p);
    while (!p->failed) {
        skip_spaces(p);
        if (p->index >= p->line_end) break;
        char op = p->text[p->index];
        // If it reaches not an addition or substraction sign it breaks to handle correctly the order of operations
        if (op != '+' && op != '-') break;
        p->index++;
        Value right = parse_term(p);
        if (p->failed) break;
        if (op == '+') left.value += right.value;
        else left.value -= right.value;
        left.is_float = left.is_float || right.is_float;
        if (!isfinite(left.value)) set_error(p, p->index);
    }
    return left;
}

static char *read_file(FILE *fp, size_t *length) {
    size_t capacity = 4096, used = 0;
    char *data = malloc(capacity);
    if (data == NULL) return NULL;
    int ch;
    while ((ch = fgetc(fp)) != EOF) {
        if (used == capacity) {
            if (capacity > (size_t)-1 / 2) { free(data); return NULL; }
            capacity *= 2;
            char *next = realloc(data, capacity);
            if (next == NULL) { free(data); return NULL; }
            data = next;
        }
        data[used++] = (char)ch;
    }
    if (ferror(fp)) { free(data); return NULL; }
    *length = used;
    return data;
}

static int evaluate_file(const char *input_name, const char *output_path) {
    FILE *in = fopen(input_name, "r");
    if (in == NULL) { perror(input_name); return 1; }
    size_t length = 0;
    char *data = read_file(in, &length);
    if (fclose(in) == EOF) { perror(input_name); free(data); return 1; }
    if (data == NULL) { fprintf(stderr, "Could not read %s\n", input_name); return 1; }

    size_t cap = 128, count = 0;
    Value *results = malloc(cap * sizeof(*results));
    if (results == NULL) { free(data); fprintf(stderr, "Out of memory\n"); return 1; }
    size_t first_error = 0;
    size_t start = 0;
    while (start < length) {
        size_t end = start;
        while (end < length && data[end] != '\n') end++;
        size_t first = start;
        while (first < end && (data[first] == ' ' || data[first] == '\t' || data[first] == '\r')) first++;
        if (first < end && data[first] != '#') {
            Parser p = {data, length, start, end, 0, 0};
            Value result = parse_expr(&p);
            skip_spaces(&p);
            if (!p.failed && p.index < end) set_error(&p, p.index + 1);
            if (p.failed) {
                first_error = p.place_error;
                break;
            }
            if (count == cap) {
                cap *= 2;
                Value *next = realloc(results, cap * sizeof(*results));
                if (next == NULL) { free(results); free(data); fprintf(stderr, "Out of memory\n"); return 1; }
                results = next;
            }
            results[count++] = result;
        }
        start = end + 1;
    }

    if (!first_error && length > 0 && data[length - 1] != '\n') {
        size_t last_start = length;
        while (last_start > 0 && data[last_start - 1] != '\n') last_start--;
        size_t first = last_start;
        while (first < length && (data[first] == ' ' || data[first] == '\t' || data[first] == '\r')) first++;
        if (first < length && data[first] != '#') {
        }
    }

    if (first_error == 0) {
        start = 0;
        while (start < length) {
            size_t end = start;
            while (end < length && data[end] != '\n') end++;
            size_t first = start;
            while (first < end && (data[first] == ' ' || data[first] == '\t' || data[first] == '\r')) first++;
            if (first < end && data[first] != '#') {
                Parser p = {data, length, start, end, 0, 0};
                (void)parse_expr(&p);
                skip_spaces(&p);
                if (!p.failed && p.index < end) set_error(&p, p.index + 1);
                if (p.failed) { first_error = p.place_error; break; }
            }
            start = end + 1;
        }
    }

    FILE *out = fopen(output_path, "w");
    if (out == NULL) { perror(output_path); free(results); free(data); return 1; }
    if (first_error != 0) {
        fprintf(out, "ERROR:%zu\n", first_error);
    } else {
        for (size_t i = 0; i < count; i++) {
            if (results[i].is_float) fprintf(out, "%.15g\n", results[i].value);
            else fprintf(out, "%.0f\n", results[i].value);
        }
    }
    int failed = ferror(out);
    if (fclose(out) == EOF) failed = 1;
    free(results);
    free(data);
    if (failed) { fprintf(stderr, "Error writing %s\n", output_path); return 1; }
    return 0;
}

static int make_output_name(const char *input, char *output, size_t size) {
    const char *base = strrchr(input, '/');
    base = base == NULL ? input : base + 1;
    size_t n = strlen(base);
    if (n >= 4 && strcmp(base + n - 4, ".txt") == 0) n -= 4;
    char shortname[512];
    if (n >= sizeof(shortname)) return 0;
    memcpy(shortname, base, n);
    shortname[n] = '\0';
    int written = snprintf(output, size, "%s_%s_%s_%s.txt", shortname, NAME, LASTNAME, STUDENTID);
    return written >= 0 && (size_t)written < size;
}

static int is_txt_file(const char *name) {
    size_t n = strlen(name);
    return n >= 4 && strcmp(name + n - 4, ".txt") == 0;
}

static int process_directory(const char *dir_name, const char *out_dir) {
    DIR *dir = opendir(dir_name);
    if (dir == NULL) { perror(dir_name); return 1; }
    struct dirent *entry;
    int status = 0;
    while ((entry = readdir(dir)) != NULL) {
        if (!is_txt_file(entry->d_name)) continue;
        char input[1024], output[1024], filename[600];
        struct stat st;
        if (snprintf(input, sizeof(input), "%s/%s", dir_name, entry->d_name) >= (int)sizeof(input)) { status = 1; continue; }
        if (stat(input, &st) != 0 || !S_ISREG(st.st_mode)) continue;
        if (!make_output_name(entry->d_name, filename, sizeof(filename))) { status = 1; continue; }
        if (snprintf(output, sizeof(output), "%s/%s", out_dir, filename) >= (int)sizeof(output)) { status = 1; continue; }
        if (evaluate_file(input, output) != 0) status = 1;
    }
    if (closedir(dir) != 0) status = 1;
    return status;
}

int main(int argc, char *argv[]) {
    const char *input = NULL;
    const char *dir_name = NULL;
    const char *out_dir = NULL;
    char default_dir[1024];
    char output[1200];

    // Checks that the command in terminal is correct
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) {
            if (i + 1 >= argc || out_dir != NULL) { fprintf(stderr, "Invalid output directory option\n"); return 1; }
            out_dir = argv[++i];
        } else if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
            if (i + 1 >= argc || dir_name != NULL) { fprintf(stderr, "Invalid input directory option\n"); return 1; }
            dir_name = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            return 1;
        } else if (input == NULL) input = argv[i];
        else { fprintf(stderr, "Too many input files\n"); return 1; }
    }

    if ((dir_name == NULL && input == NULL) || (dir_name != NULL && input != NULL)) {
        fprintf(stderr, "Usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
        return 1;
    }

    if (out_dir != NULL) {
        struct stat st;
        if (stat(out_dir, &st) != 0) {
            if (mkdir(out_dir, 0777) != 0) { perror(out_dir); return 1; }
        } else if (!S_ISDIR(st.st_mode)) {
            fprintf(stderr, "%s is not a directory\n", out_dir); return 1;
        }
    } else {
        const char *base = dir_name != NULL ? strrchr(dir_name, '/') : strrchr(input, '/');
        base = base == NULL ? (dir_name != NULL ? dir_name : input) : base + 1;
        size_t n = strlen(base);
        if (dir_name == NULL && n >= 4 && strcmp(base + n - 4, ".txt") == 0) n -= 4;
        if (snprintf(default_dir, sizeof(default_dir), "%.*s_%s_%s", (int)n, base, NAME, STUDENTID) >= (int)sizeof(default_dir)) {
            fprintf(stderr, "Output directory name too long\n"); return 1;
        }
        out_dir = default_dir;
        struct stat st;
        if (stat(out_dir, &st) != 0) {
            if (mkdir(out_dir, 0777) != 0) { perror(out_dir); return 1; }
        } else if (!S_ISDIR(st.st_mode)) { fprintf(stderr, "%s is not a directory\n", out_dir); return 1; }
    }

    if (dir_name != NULL) return process_directory(dir_name, out_dir);
    if (!make_output_name(input, output, sizeof(output))) { fprintf(stderr, "Output filename too long\n"); return 1; }
    char full_output[1600];
    if (snprintf(full_output, sizeof(full_output), "%s/%s", out_dir, output) >= (int)sizeof(full_output)) {
        fprintf(stderr, "Output path too long\n"); return 1;
    }
    return evaluate_file(input, full_output);
}
