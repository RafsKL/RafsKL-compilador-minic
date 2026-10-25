
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ======================================================================
 * Utilitários
 * ====================================================================== */

static void die(int code, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fputc('\n', stderr);
    exit(code);
}

static void *xmalloc(size_t n) {
    void *p = malloc(n ? n : 1);
    if (!p) die(1, "memória insuficiente");
    return p;
}

static void *xrealloc(void *p, size_t n) {
    p = realloc(p, n ? n : 1);
    if (!p) die(1, "memória insuficiente");
    return p;
}

static char *xstrndup(const char *s, size_t n) {
    char *p = xmalloc(n + 1);
    memcpy(p, s, n);
    p[n] = '\0';
    return p;
}

static char *xstrdup(const char *s) { return xstrndup(s, strlen(s)); }

static char *xvfmt(const char *fmt, va_list ap) {
    va_list ap2;
    va_copy(ap2, ap);
    int n = vsnprintf(NULL, 0, fmt, ap2);
    va_end(ap2);
    char *p = xmalloc((size_t)n + 1);
    vsnprintf(p, (size_t)n + 1, fmt, ap);
    return p;
}

static char *xfmt(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    char *p = xvfmt(fmt, ap);
    va_end(ap);
    return p;
}

/* Vetor dinâmico de ponteiros */
typedef struct {
    void **v;
    int n, cap;
} Vec;

static void vec_push(Vec *a, void *x) {
    if (a->n == a->cap) {
        a->cap = a->cap ? a->cap * 2 : 8;
        a->v = xrealloc(a->v, (size_t)a->cap * sizeof(void *));
    }
    a->v[a->n++] = x;
}

static char *strip(const char *s) {
    while (isspace((unsigned char)*s)) s++;
    size_t n = strlen(s);
    while (n && isspace((unsigned char)s[n - 1])) n--;
    return xstrndup(s, n);
}

static Vec split_ws(const char *s) {
    Vec v = {0};
    while (*s) {
        while (*s && isspace((unsigned char)*s)) s++;
        if (!*s) break;
        const char *b = s;
        while (*s && !isspace((unsigned char)*s)) s++;
        vec_push(&v, xstrndup(b, (size_t)(s - b)));
    }
    return v;
}

/* ======================================================================
 * Léxico
 * ====================================================================== */

enum {
    /* Tokens de um caractere usam o próprio código ASCII como tipo */
    T_EOF = 256, T_TYPE, T_IF, T_ELSE, T_WHILE, T_RETURN, T_PRINT, T_BREAK, T_FOR,
    T_BOOL_LIT, T_ID, T_INT_LIT, T_REAL_LIT, T_LIT,
    T_EQ, T_NE, T_LE, T_GE, T_AND, T_OR
};

typedef struct {
    int kind;
    char *value;
    int line, col;
} Tok;

typedef struct {
    Tok *v;
    int n, cap;
} TokVec;

static const struct { const char *w; int kind; } KEYWORDS[] = {
    {"int", T_TYPE}, {"float", T_TYPE}, {"bool", T_TYPE}, {"void", T_TYPE},
    {"if", T_IF}, {"else", T_ELSE}, {"while", T_WHILE}, {"return", T_RETURN},
    {"print", T_PRINT}, {"break", T_BREAK}, {"for", T_FOR},
    {"true", T_BOOL_LIT}, {"false", T_BOOL_LIT},
};

static const struct { const char *s; int kind; } MULTI[] = {
    {"==", T_EQ}, {"!=", T_NE}, {"<=", T_LE}, {">=", T_GE}, {"&&", T_AND}, {"||", T_OR},
};

static const char SINGLE[] = "+-*/%<>=!(){}[];,";

static void tok_push(TokVec *a, int kind, char *value, int line, int col) {
    if (a->n == a->cap) {
        a->cap = a->cap ? a->cap * 2 : 64;
        a->v = xrealloc(a->v, (size_t)a->cap * sizeof(Tok));
    }
    a->v[a->n++] = (Tok){kind, value, line, col};
}

typedef struct {
    const char *src;
    size_t i, len;
    int line, col;
} Lexer;

static char lx_adv(Lexer *L) {
    char c = L->src[L->i++];
    if (c == '\n') { L->line++; L->col = 1; }
    else L->col++;
    return c;
}

static TokVec lex(const char *src) {
    TokVec out = {0};
    Lexer L = {src, 0, strlen(src), 1, 1};

    while (L.i < L.len) {
        unsigned char c = (unsigned char)src[L.i];
        if (isspace(c)) { lx_adv(&L); continue; }

        int l = L.line, cc = L.col;
        size_t st = L.i;

        if (isalpha(c) || c == '_') {
            lx_adv(&L);
            while (L.i < L.len && (isalnum((unsigned char)src[L.i]) || src[L.i] == '_')) lx_adv(&L);
            char *v = xstrndup(src + st, L.i - st);
            int kind = T_ID;
            for (size_t k = 0; k < sizeof KEYWORDS / sizeof *KEYWORDS; k++)
                if (!strcmp(v, KEYWORDS[k].w)) { kind = KEYWORDS[k].kind; break; }
            tok_push(&out, kind, v, l, cc);
            continue;
        }

        if (isdigit(c)) {
            int kind = T_INT_LIT;
            lx_adv(&L);
            while (L.i < L.len && isdigit((unsigned char)src[L.i])) lx_adv(&L);
            if (L.i < L.len && src[L.i] == '.') {
                lx_adv(&L);
                while (L.i < L.len && isdigit((unsigned char)src[L.i])) lx_adv(&L);
                kind = T_REAL_LIT;
            }
            tok_push(&out, kind, xstrndup(src + st, L.i - st), l, cc);
            continue;
        }

        int op = 0;
        for (size_t k = 0; k < sizeof MULTI / sizeof *MULTI; k++)
            if (!strncmp(src + L.i, MULTI[k].s, 2)) { op = MULTI[k].kind; break; }
        if (op) {
            lx_adv(&L); lx_adv(&L);
            tok_push(&out, op, xstrndup(src + st, 2), l, cc);
            continue;
        }

        if (c == '\'' || c == '"') {
            char q = (char)c;
            lx_adv(&L);
            while (L.i < L.len) {
                char x = lx_adv(&L);
                if (x == '\\' && L.i < L.len) { lx_adv(&L); continue; }
                if (x == q) break;
            }
            tok_push(&out, T_LIT, xstrndup(src + st, L.i - st), l, cc);
            continue;
        }

        if (c && strchr(SINGLE, c)) {
            lx_adv(&L);
            tok_push(&out, c, xstrndup(src + st, 1), l, cc);
            continue;
        }

        die(2, "Erro léxico: caractere inesperado '%c' na linha %d, coluna %d", c, L.line, L.col);
    }
    tok_push(&out, T_EOF, xstrdup(""), L.line, L.col);
    return out;
}

static const char *kind_name(int k) {
    static char buf[2];
    switch (k) {
    case T_EOF: return "EOF";
    case T_TYPE: return "TYPE";
    case T_ID: return "ID";
    case T_IF: return "IF";
    case T_ELSE: return "ELSE";
    case T_WHILE: return "WHILE";
    case T_RETURN: return "RETURN";
    case T_PRINT: return "PRINT";
    case T_BREAK: return "BREAK";
    case T_FOR: return "FOR";
    case T_EQ: return "==";
    case T_NE: return "!=";
    case T_LE: return "<=";
    case T_GE: return ">=";
    case T_AND: return "&&";
    case T_OR: return "||";
    default: buf[0] = (char)k; buf[1] = '\0'; return buf;
    }
}

/* ======================================================================
 * AST
 * ====================================================================== */

typedef enum {
    N_FUNC, N_DECL, N_BLOCK, N_IF, N_WHILE, N_FOR, N_PRINT, N_BREAK, N_RETURN,
    N_EXPRSTMT, N_ASSIGN, N_BINARY, N_UNARY, N_INDEX, N_CALL, N_ID, N_LIT
} NodeKind;

typedef enum { Y_INT, Y_FLOAT, Y_BOOL, Y_VOID, Y_ERROR } Base;

static Base base_of(const char *s) {
    if (!strcmp(s, "int")) return Y_INT;
    if (!strcmp(s, "float")) return Y_FLOAT;
    if (!strcmp(s, "bool")) return Y_BOOL;
    return Y_VOID;
}

typedef struct {
    Base typ;
    char *name;
    Tok *tok;
    bool array;
} Param;

typedef struct Node Node;
struct Node {
    NodeKind k;
    Tok *tok;                       /* token de referência (linha para erros) */

    /* func / decl */
    Base typ;
    char *name;
    Tok *nameTok;
    Param *params;
    int nparams;
    Node *size;                     /* decl: tamanho do vetor */
    Node *init;                     /* decl: inicializador | for: inicialização */
    Node *body;

    /* block */
    Node **items;
    int nitems;

    /* if / while / for */
    Node *cond, *yes, *no, *inc;

    /* expressões e comandos simples */
    Node *left, *right, *expr, *base, *index, *callee, *retval;
    Node **args;
    int nargs;
    const char *op;                 /* texto do operador */
    int opk;                        /* tipo do token do operador */
    Base litType;
    const char *lit;                /* texto do literal */

    char *ir_name;                  /* preenchido pela análise semântica */
};

static Node *mk(NodeKind k, Tok *tok) {
    Node *n = xmalloc(sizeof *n);
    memset(n, 0, sizeof *n);
    n->k = k;
    n->tok = tok;
    return n;
}

/* ======================================================================
 * Parser
 * ====================================================================== */

typedef struct {
    TokVec ts;
    int i;
} Parser;

static Tok *cur(Parser *p) { return &p->ts.v[p->i]; }

static Tok *take(Parser *p, int k) { /* k == 0: aceita qualquer token */
    Tok *t = cur(p);
    if (k && t->kind != k)
        die(3, "Erro sintático: esperado %s, encontrado %s na linha %d",
            kind_name(k), t->value[0] ? t->value : "fim do arquivo", t->line);
    p->i++;
    return t;
}

static Tok *match(Parser *p, int k) {
    return cur(p)->kind == k ? take(p, 0) : NULL;
}

static Node *expression(Parser *p);
static Node *assignment(Parser *p);
static Node *statement(Parser *p);
static Node *block(Parser *p);

static Node *finish_decl(Parser *p, Tok *typ, Tok *name) {
    Node *n = mk(N_DECL, typ);
    n->typ = base_of(typ->value);
    n->name = name->value;
    n->nameTok = name;
    if (match(p, '[')) { n->size = expression(p); take(p, ']'); }
    else if (match(p, '=')) n->init = expression(p);
    take(p, ';');
    return n;
}

static Param parse_param(Parser *p) {
    Tok *typ = take(p, T_TYPE);
    Tok *name = take(p, T_ID);
    bool arr = match(p, '[') != NULL;
    if (arr) take(p, ']');
    return (Param){base_of(typ->value), name->value, name, arr};
}

static Vec parse_program(Parser *p) {
    Vec xs = {0};
    while (cur(p)->kind != T_EOF) {
        Tok *typ = take(p, T_TYPE);
        Tok *name = take(p, T_ID);
        if (match(p, '(')) {
            Param *params = NULL;
            int np = 0, cap = 0;
            if (cur(p)->kind != ')') {
                if (cur(p)->kind == T_TYPE && !strcmp(cur(p)->value, "void") &&
                    p->ts.v[p->i + 1].kind == ')') {
                    take(p, 0);
                } else {
                    do {
                        if (np == cap) { cap = cap ? cap * 2 : 4; params = xrealloc(params, (size_t)cap * sizeof(Param)); }
                        params[np++] = parse_param(p);
                    } while (match(p, ','));
                }
            }
            take(p, ')');
            Node *body = block(p);
            Node *f = mk(N_FUNC, typ);
            f->typ = base_of(typ->value);
            f->name = name->value;
            f->nameTok = name;
            f->params = params;
            f->nparams = np;
            f->body = body;
            vec_push(&xs, f);
        } else {
            vec_push(&xs, finish_decl(p, typ, name));
        }
    }
    return xs;
}

static Node *block(Parser *p) {
    Tok *st = take(p, '{');
    Vec xs = {0};
    while (cur(p)->kind != '}' && cur(p)->kind != T_EOF) {
        if (cur(p)->kind == T_TYPE) {
            Tok *typ = take(p, 0);
            Tok *name = take(p, T_ID);
            vec_push(&xs, finish_decl(p, typ, name));
        } else {
            vec_push(&xs, statement(p));
        }
    }
    take(p, '}');
    Node *n = mk(N_BLOCK, st);
    n->items = (Node **)xs.v;
    n->nitems = xs.n;
    return n;
}

static Node *statement(Parser *p) {
    Tok *t;
    if (cur(p)->kind == '{') return block(p);

    if ((t = match(p, T_IF))) {
        take(p, '('); Node *c = expression(p); take(p, ')');
        Node *a = statement(p);
        Node *b = match(p, T_ELSE) ? statement(p) : NULL;
        Node *n = mk(N_IF, t);
        n->cond = c; n->yes = a; n->no = b;
        return n;
    }
    if ((t = match(p, T_WHILE))) {
        take(p, '('); Node *c = expression(p); take(p, ')');
        Node *b = statement(p);
        Node *n = mk(N_WHILE, t);
        n->cond = c; n->body = b;
        return n;
    }
    if ((t = match(p, T_FOR))) {
        take(p, '(');
        Node *init = cur(p)->kind != ';' ? assignment(p) : NULL; take(p, ';');
        Node *cond = cur(p)->kind != ';' ? expression(p) : NULL; take(p, ';');
        Node *inc = cur(p)->kind != ')' ? assignment(p) : NULL; take(p, ')');
        Node *body = statement(p);
        Node *n = mk(N_FOR, t);
        n->init = init; n->cond = cond; n->inc = inc; n->body = body;
        return n;
    }
    if ((t = match(p, T_PRINT))) {
        take(p, '('); Node *x = expression(p); take(p, ')'); take(p, ';');
        Node *n = mk(N_PRINT, t);
        n->expr = x;
        return n;
    }
    if ((t = match(p, T_BREAK))) {
        take(p, ';');
        return mk(N_BREAK, t);
    }
    if ((t = match(p, T_RETURN))) {
        Node *v = cur(p)->kind == ';' ? NULL : expression(p);
        take(p, ';');
        Node *n = mk(N_RETURN, t);
        n->retval = v;
        return n;
    }
    Node *x = expression(p);
    take(p, ';');
    Node *n = mk(N_EXPRSTMT, x->tok);
    n->expr = x;
    return n;
}

static Node *expression(Parser *p) { return assignment(p); }

static Node *binary(Parser *p, int level);
static Node *unary(Parser *p);
static Node *primary(Parser *p);

static Node *assignment(Parser *p) {
    Node *x = binary(p, 0);
    if (match(p, '=')) {
        Node *y = assignment(p);
        Node *n = mk(N_ASSIGN, x->tok);
        n->left = x; n->right = y;
        return n;
    }
    return x;
}

/* Níveis de precedência: || ; && ; == != ; < > <= >= ; + - ; * / % */
static bool in_level(int lvl, int k) {
    switch (lvl) {
    case 0: return k == T_OR;
    case 1: return k == T_AND;
    case 2: return k == T_EQ || k == T_NE;
    case 3: return k == '<' || k == '>' || k == T_LE || k == T_GE;
    case 4: return k == '+' || k == '-';
    case 5: return k == '*' || k == '/' || k == '%';
    }
    return false;
}

static Node *binary(Parser *p, int level) {
    if (level == 6) return unary(p);
    Node *x = binary(p, level + 1);
    while (in_level(level, cur(p)->kind)) {
        Tok *op = take(p, 0);
        Node *y = binary(p, level + 1);
        Node *n = mk(N_BINARY, x->tok);
        n->op = op->value; n->opk = op->kind;
        n->left = x; n->right = y;
        x = n;
    }
    return x;
}

static Node *unary(Parser *p) {
    int k = cur(p)->kind;
    if (k == '!' || k == '+' || k == '-') {
        Tok *op = take(p, 0);
        Node *x = unary(p);
        Node *n = mk(N_UNARY, op);
        n->op = op->value; n->opk = op->kind;
        n->expr = x;
        return n;
    }
    Node *x = primary(p);
    for (;;) {
        if (match(p, '[')) {
            Node *idx = expression(p);
            take(p, ']');
            Node *n = mk(N_INDEX, x->tok);
            n->base = x; n->index = idx;
            x = n;
        } else if (match(p, '(')) {
            Vec args = {0};
            if (cur(p)->kind != ')') {
                vec_push(&args, expression(p));
                while (match(p, ',')) vec_push(&args, expression(p));
            }
            take(p, ')');
            Node *n = mk(N_CALL, x->tok);
            n->callee = x;
            n->args = (Node **)args.v;
            n->nargs = args.n;
            x = n;
        } else {
            break;
        }
    }
    return x;
}

static Node *make_lit(Tok *t, Base ty) {
    Node *n = mk(N_LIT, t);
    n->litType = ty;
    n->lit = t->value;
    return n;
}

static Node *primary(Parser *p) {
    Tok *t = cur(p);
    if (match(p, T_ID)) {
        Node *n = mk(N_ID, t);
        n->name = t->value;
        return n;
    }
    if (match(p, T_INT_LIT)) return make_lit(t, Y_INT);
    if (match(p, T_REAL_LIT)) return make_lit(t, Y_FLOAT);
    if (match(p, T_BOOL_LIT)) return make_lit(t, Y_BOOL);
    if (match(p, '(')) {
        Node *x = expression(p);
        take(p, ')');
        return x;
    }
    die(3, "Erro sintático: expressão inesperada %s na linha %d", t->value[0] ? t->value : "fim", t->line);
    return NULL;
}

/* ======================================================================
 * Análise semântica
 * ====================================================================== */

/* Tipo: base + flag de vetor (equivale a 'int' vs 'array:int' no Python) */
typedef struct {
    Base base;
    bool arr;
} Ty;

static Ty ty(Base b) { return (Ty){b, false}; }
static bool is_ty(Ty t, Base b) { return !t.arr && t.base == b; }

typedef struct {
    Ty ty;
    char *ir_name;
} Sym;

typedef struct {
    char *name;
    Sym sym;
} Entry;

typedef struct {
    Entry *e;
    int n, cap;
} Scope;

static Sym *scope_find(Scope *s, const char *name) {
    for (int i = 0; i < s->n; i++)
        if (!strcmp(s->e[i].name, name)) return &s->e[i].sym;
    return NULL;
}

static void scope_set(Scope *s, char *name, Sym sym) {
    Sym *old = scope_find(s, name);
    if (old) { *old = sym; return; }
    if (s->n == s->cap) {
        s->cap = s->cap ? s->cap * 2 : 8;
        s->e = xrealloc(s->e, (size_t)s->cap * sizeof(Entry));
    }
    s->e[s->n++] = (Entry){name, sym};
}

typedef struct {
    char *name;
    Base ret;
    int nparams;
} FuncSig;

typedef struct {
    char *name;
    int n;
} Cnt;

typedef struct {
    Vec errors;      /* char* */
    Scope globals;
    Vec funcs;       /* FuncSig* */
    Vec decl_cnt;    /* Cnt*: declarações já vistas na função atual, por nome */
    Node *cur_body;  /* corpo da função em análise */
} Sem;

/* Quantas vezes o nome é declarado (em qualquer bloco) dentro de n */
static int count_decls(Node *n, const char *name) {
    if (!n) return 0;
    int c = 0;
    switch (n->k) {
    case N_DECL: return !strcmp(n->name, name);
    case N_BLOCK: for (int i = 0; i < n->nitems; i++) c += count_decls(n->items[i], name); return c;
    case N_IF: return count_decls(n->yes, name) + count_decls(n->no, name);
    case N_WHILE: case N_FOR: return count_decls(n->body, name);
    default: return 0;
    }
}

static Cnt *cnt_get(Sem *S, const char *name) {
    for (int i = 0; i < S->decl_cnt.n; i++) {
        Cnt *c = S->decl_cnt.v[i];
        if (!strcmp(c->name, name)) return c;
    }
    Cnt *c = xmalloc(sizeof *c);
    c->name = xstrdup(name);
    c->n = 0;
    vec_push(&S->decl_cnt, c);
    return c;
}

static void sem_add(Sem *S, Tok *tok, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    char *msg = xvfmt(fmt, ap);
    va_end(ap);
    vec_push(&S->errors, xfmt("Erro semântico (linha %d): %s", tok->line, msg));
}

static FuncSig *find_func(Sem *S, const char *name) {
    if (!name) return NULL;
    for (int i = 0; i < S->funcs.n; i++) {
        FuncSig *f = S->funcs.v[i];
        if (!strcmp(f->name, name)) return f;
    }
    return NULL;
}

static Sym *lookup(Sem *S, const char *name, Vec *scopes) {
    if (!name) return NULL;
    for (int i = scopes->n - 1; i >= 0; i--) {
        Sym *s = scope_find(scopes->v[i], name);
        if (s) return s;
    }
    return scope_find(&S->globals, name);
}

static bool compatible(Base dst, Ty src) {
    return !src.arr && (src.base == dst || (dst == Y_FLOAT && src.base == Y_INT));
}

static Ty check_expr(Sem *S, Node *n, Vec *scopes) {
    if (!n) return ty(Y_VOID);
    switch (n->k) {
    case N_LIT:
        return ty(n->litType);

    case N_ID: {
        Sym *sym = lookup(S, n->name, scopes);
        if (!sym) {
            sem_add(S, n->tok, "Identificador não declarado: %s", n->name);
            return ty(Y_ERROR);
        }
        n->ir_name = sym->ir_name;
        return sym->ty;
    }

    case N_INDEX: {
        Ty bt = check_expr(S, n->base, scopes);
        Ty it = check_expr(S, n->index, scopes);
        if (!is_ty(it, Y_INT) && !is_ty(it, Y_ERROR))
            sem_add(S, n->index->tok, "Índice de vetor deve ser int.");
        if (!bt.arr && bt.base != Y_ERROR)
            sem_add(S, n->base->tok, "Indexação de variável escalar não permitida.");
        return bt.arr ? ty(bt.base) : ty(Y_ERROR);
    }

    case N_CALL: {
        const char *name = n->callee->k == N_ID ? n->callee->name : NULL;
        for (int i = 0; i < n->nargs; i++) check_expr(S, n->args[i], scopes);
        FuncSig *f = find_func(S, name);
        if (!f) {
            Sym *sym = lookup(S, name, scopes);
            if (sym) sem_add(S, n->callee->tok, "%s é uma variável, não função.", name);
            else sem_add(S, n->callee->tok, "Função %s não declarada.", name ? name : "None");
            return ty(Y_ERROR);
        }
        if (n->nargs != f->nparams) sem_add(S, n->tok, "Aridade incorreta para %s.", name);
        return ty(f->ret);
    }

    case N_ASSIGN: {
        Ty lt = check_expr(S, n->left, scopes);
        Ty rt = check_expr(S, n->right, scopes);
        lt.arr = false;
        if (is_ty(lt, Y_INT) && is_ty(rt, Y_FLOAT)) sem_add(S, n->tok, "Não é possível atribuir float a int.");
        if (is_ty(lt, Y_INT) && is_ty(rt, Y_BOOL)) sem_add(S, n->tok, "Não é possível atribuir bool a int.");
        return lt;
    }

    case N_UNARY: {
        Ty t = check_expr(S, n->expr, scopes);
        return n->opk == '!' ? ty(Y_BOOL) : t;
    }

    case N_BINARY: {
        Ty a = check_expr(S, n->left, scopes);
        Ty b = check_expr(S, n->right, scopes);
        switch (n->opk) {
        case '+': case '-': case '*': case '/': case '%':
            if (is_ty(a, Y_BOOL) || is_ty(b, Y_BOOL))
                sem_add(S, n->tok, "Aritmética envolvendo bool não é permitida.");
            return (is_ty(a, Y_FLOAT) || is_ty(b, Y_FLOAT)) ? ty(Y_FLOAT) : a;
        default: /* &&, ||, <, >, <=, >=, ==, != */
            return ty(Y_BOOL);
        }
    }

    default:
        return ty(Y_ERROR);
    }
}

static void check_stmt(Sem *S, Node *n, Vec *scopes, Base fn_ret, bool in_loop) {
    switch (n->k) {
    case N_BLOCK: {
        Scope *scope = xmalloc(sizeof *scope);
        memset(scope, 0, sizeof *scope);
        vec_push(scopes, scope);
        for (int i = 0; i < n->nitems; i++) {
            Node *st = n->items[i];
            if (st->k != N_DECL) {
                check_stmt(S, st, scopes, fn_ret, in_loop);
                continue;
            }
            if (st->typ == Y_VOID) sem_add(S, st->nameTok, "Variável não pode ser void.");
            if (scope_find(scope, st->name)) {
                sem_add(S, st->nameTok, "Declaração duplicada.");
            } else {
                /* 1ª declaração do nome na função mantém o nome (x#outer se for sombreada);
                 * as seguintes viram x#inner, x#inner2, ... */
                Cnt *cn = cnt_get(S, st->name);
                cn->n++;
                char *ir_name;
                if (cn->n == 1)
                    ir_name = count_decls(S->cur_body, st->name) > 1 ? xfmt("%s#outer", st->name) : st->name;
                else if (cn->n == 2)
                    ir_name = xfmt("%s#inner", st->name);
                else
                    ir_name = xfmt("%s#inner%d", st->name, cn->n - 1);
                scope_set(scope, st->name, (Sym){(Ty){st->typ, st->size != NULL}, ir_name});
                st->ir_name = ir_name;
            }
            if (st->init) {
                Ty rt = check_expr(S, st->init, scopes);
                if (st->typ == Y_INT && is_ty(rt, Y_FLOAT)) sem_add(S, st->nameTok, "Não é possível atribuir float a int.");
                if (st->typ == Y_INT && is_ty(rt, Y_BOOL)) sem_add(S, st->nameTok, "Não é possível atribuir bool a int.");
            }
        }
        scopes->n--;
        break;
    }
    case N_RETURN: {
        Node *v = n->retval;
        Ty t = v ? check_expr(S, v, scopes) : ty(Y_VOID);
        if (v && fn_ret == Y_VOID) sem_add(S, n->tok, "return com valor em função void.");
        else if (!v && fn_ret != Y_VOID) sem_add(S, n->tok, "retorno sem valor em função não void.");
        else if (fn_ret != Y_VOID && !is_ty(t, Y_ERROR) && !compatible(fn_ret, t))
            sem_add(S, n->tok, "Retorno incompatível.");
        break;
    }
    case N_IF:
        check_expr(S, n->cond, scopes);
        check_stmt(S, n->yes, scopes, fn_ret, in_loop);
        if (n->no) check_stmt(S, n->no, scopes, fn_ret, in_loop);
        break;
    case N_WHILE:
        check_expr(S, n->cond, scopes);
        check_stmt(S, n->body, scopes, fn_ret, true);
        break;
    case N_FOR:
        if (n->init) check_expr(S, n->init, scopes);
        if (n->cond) check_expr(S, n->cond, scopes);
        if (n->inc) check_expr(S, n->inc, scopes);
        check_stmt(S, n->body, scopes, fn_ret, true);
        break;
    case N_BREAK:
        if (!in_loop) sem_add(S, n->tok, "break só pode ocorrer dentro de laços.");
        break;
    case N_PRINT:
    case N_EXPRSTMT:
        check_expr(S, n->expr, scopes);
        break;
    default:
        break;
    }
}

/* Retorna a lista de mensagens de erro (vazia = programa válido) */
static Vec run_semantic_analysis(Vec *tree) {
    Sem S;
    memset(&S, 0, sizeof S);

    for (int i = 0; i < tree->n; i++) {
        Node *n = tree->v[i];
        if (n->k == N_FUNC) {
            if (scope_find(&S.globals, n->name) || find_func(&S, n->name))
                sem_add(&S, n->nameTok, "Colisão: %s já declarado.", n->name);
            FuncSig *f = find_func(&S, n->name);
            if (!f) {
                f = xmalloc(sizeof *f);
                f->name = n->name;
                vec_push(&S.funcs, f);
            }
            f->ret = n->typ;
            f->nparams = n->nparams;
        } else {
            if (n->typ == Y_VOID) sem_add(&S, n->nameTok, "Variável não pode ser do tipo void.");
            scope_set(&S.globals, n->name, (Sym){(Ty){n->typ, n->size != NULL}, n->name});
        }
    }

    for (int i = 0; i < tree->n; i++) {
        Node *n = tree->v[i];
        if (n->k != N_FUNC) continue;
        Scope *scope = xmalloc(sizeof *scope);
        memset(scope, 0, sizeof *scope);
        for (int j = 0; j < n->nparams; j++) {
            Param *p = &n->params[j];
            scope_set(scope, p->name, (Sym){(Ty){p->typ, p->array}, p->name});
        }
        Vec scopes = {0};
        vec_push(&scopes, scope);
        S.decl_cnt.n = 0;
        S.cur_body = n->body;
        check_stmt(&S, n->body, &scopes, n->typ, false);
    }
    return S.errors;
}

/* ======================================================================
 * Gerador de IR
 * ====================================================================== */

typedef struct {
    Vec code;      /* char* */
    int temp_cnt;
    const char *lab_pref[16]; /* prefixos de rótulo já usados na função */
    int lab_cnt[16];
    int nlab;
    bool optimize;
} IRGen;

static void emit(IRGen *G, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vec_push(&G->code, xvfmt(fmt, ap));
    va_end(ap);
}

static char *new_temp(IRGen *G) { return xfmt("t%d", G->temp_cnt++); }
/* Rótulos são locais à função: o primeiro de cada tipo é L_end, depois L_end_2, L_end_3... */
static char *new_label(IRGen *G, const char *pref) {
    for (int i = 0; i < G->nlab; i++)
        if (!strcmp(G->lab_pref[i], pref)) {
            int c = ++G->lab_cnt[i];
            return xfmt("%s_%d", pref, c);
        }
    if (G->nlab == 16) die(1, "Erro interno: tipos de rótulo demais");
    G->lab_pref[G->nlab] = pref;
    G->lab_cnt[G->nlab++] = 1;
    return xstrdup(pref);
}

static const char *need_ir_name(Node *n) {
    if (!n->ir_name) die(1, "Erro interno: identificador sem nome de IR (linha %d)", n->tok->line);
    return n->ir_name;
}

static char *gen_expr(IRGen *G, Node *n) {
    if (!n) return xstrdup("");
    switch (n->k) {
    case N_LIT:
        return xstrdup(n->lit);

    case N_ID:
        return xstrdup(need_ir_name(n));

    case N_ASSIGN: {
        char *r = gen_expr(G, n->right);
        if (n->left->k == N_ID) {
            const char *lname = need_ir_name(n->left);
            emit(G, "  %s = %s", lname, r);
            return xstrdup(lname);
        }
        if (n->left->k == N_INDEX) {
            const char *base = need_ir_name(n->left->base);
            char *idx = gen_expr(G, n->left->index);
            char *t = new_temp(G);
            emit(G, "  %s = %s * 1", t, idx);
            emit(G, "  store %s[%s], %s", base, t, r);
            return r;
        }
        return xstrdup("None"); /* atribuição a algo que não é lvalue (igual ao Python) */
    }

    case N_INDEX: {
        const char *base = need_ir_name(n->base);
        char *idx = gen_expr(G, n->index);
        char *t1 = new_temp(G);
        char *t2 = new_temp(G);
        emit(G, "  %s = %s * 1", t1, idx);
        emit(G, "  %s = load %s[%s]", t2, base, t1);
        return t2;
    }

    case N_UNARY: {
        char *e = gen_expr(G, n->expr);
        char *t = new_temp(G);
        emit(G, "  %s = %s %s", t, n->opk == '!' ? "not" : n->op, e);
        return t;
    }

    case N_BINARY: {
        char *l = gen_expr(G, n->left);
        char *r = gen_expr(G, n->right);
        char *t = new_temp(G);
        const char *op = n->op;
        if (n->opk == T_AND) op = "and";
        if (n->opk == T_OR) op = "or";
        emit(G, "  %s = %s %s %s", t, l, op, r);
        return t;
    }

    case N_CALL: {
        for (int i = 0; i < n->nargs; i++) {
            char *ar = gen_expr(G, n->args[i]);
            emit(G, "  param %s", ar);
        }
        char *t = new_temp(G);
        emit(G, "  %s = call %s, %d", t, n->callee->name, n->nargs);
        return t;
    }

    default:
        return xstrdup("");
    }
}

static void gen_stmt(IRGen *G, Node *n, const char *l_break) {
    switch (n->k) {
    case N_BLOCK:
        for (int i = 0; i < n->nitems; i++) gen_stmt(G, n->items[i], l_break);
        break;

    case N_DECL:
        if (n->init) {
            char *r = gen_expr(G, n->init);
            emit(G, "  %s = %s", n->ir_name, r);
        }
        break;

    case N_EXPRSTMT:
        gen_expr(G, n->expr);
        break;

    case N_PRINT: {
        char *v = gen_expr(G, n->expr);
        emit(G, "  print %s", v);
        break;
    }

    case N_RETURN:
        if (n->retval) {
            char *v = gen_expr(G, n->retval);
            emit(G, "  return %s", v);
        } else {
            emit(G, "  return");
        }
        break;

    case N_IF: {
        char *c = gen_expr(G, n->cond);
        if (!n->no) {
            char *l_end = new_label(G, "L_end_if");
            emit(G, "  ifFalse %s goto %s", c, l_end);
            gen_stmt(G, n->yes, l_break);
            emit(G, "%s:", l_end);
            break;
        }
        char *l_el = new_label(G, "L_else");
        char *l_end = new_label(G, "L_end");
        emit(G, "  ifFalse %s goto %s", c, l_el);
        gen_stmt(G, n->yes, l_break);
        emit(G, "  goto %s", l_end);
        emit(G, "%s:", l_el);
        gen_stmt(G, n->no, l_break);
        emit(G, "%s:", l_end);
        break;
    }

    case N_WHILE: {
        char *l_test = new_label(G, "L_test");
        char *l_end = new_label(G, "L_end");
        emit(G, "%s:", l_test);
        char *c = gen_expr(G, n->cond);
        emit(G, "  ifFalse %s goto %s", c, l_end);
        gen_stmt(G, n->body, l_end);
        emit(G, "  goto %s", l_test);
        emit(G, "%s:", l_end);
        break;
    }

    case N_FOR: {
        if (n->init) gen_expr(G, n->init);
        char *l_test = new_label(G, "L_test");
        char *l_end = new_label(G, "L_end");
        emit(G, "%s:", l_test);
        if (n->cond) {
            char *c = gen_expr(G, n->cond);
            emit(G, "  ifFalse %s goto %s", c, l_end);
        }
        gen_stmt(G, n->body, l_end);
        if (n->inc) gen_expr(G, n->inc);
        emit(G, "  goto %s", l_test);
        emit(G, "%s:", l_end);
        break;
    }

    case N_BREAK:
        if (l_break) emit(G, "  goto %s", l_break);
        break;

    default:
        break;
    }
}

static void gen_func(IRGen *G, Node *n) {
    G->temp_cnt = 1; /* t1, t2, ... e os rótulos reiniciam em cada função */
    G->nlab = 0;
    if (n->nparams) {
        char *list = xstrdup("");
        for (int i = 0; i < n->nparams; i++)
            list = xfmt("%s%s%s", list, i ? ", " : "", n->params[i].name);
        emit(G, "func %s(%s)", n->name, list);
    } else {
        emit(G, "func %s", n->name);
    }
    gen_stmt(G, n->body, NULL);
    emit(G, "endfunc");
}

/* ----------------------------------------------------------------------
 * Otimizador (-O1): propagação e dobra de constantes sobre o texto da IR.
 * O Python usa eval(); aqui há um avaliador mínimo com a mesma semântica
 * para os formatos que a IR produz ("a", "op a" e "a op b").
 * Limitação: inteiros além de 64 bits não são avaliados (a linha fica como está).
 * ---------------------------------------------------------------------- */

typedef enum { V_INT, V_FLOAT, V_BOOL } VK;
typedef struct {
    VK k;
    long long i;   /* int e bool */
    double d;      /* float */
} Val;

static Val mk_int(long long i) { return (Val){V_INT, i, 0}; }
static Val mk_float(double d) { return (Val){V_FLOAT, 0, d}; }
static Val mk_bool(bool b) { return (Val){V_BOOL, b, 0}; }
static double as_d(Val v) { return v.k == V_FLOAT ? v.d : (double)v.i; }
static bool truthy(Val v) { return v.k == V_FLOAT ? v.d != 0 : v.i != 0; }

/* Literal numérico/booleano no formato que str() do Python produz */
static bool parse_val(const char *s, Val *out) {
    if (!strcmp(s, "true")) { *out = mk_bool(true); return true; }
    if (!strcmp(s, "false")) { *out = mk_bool(false); return true; }

    const char *p = s;
    if (*p == '-') p++;
    const char *d0 = p;
    while (isdigit((unsigned char)*p)) p++;
    size_t nd = (size_t)(p - d0);
    if (nd == 0) return false;

    bool isf = false;
    if (*p == '.') {
        isf = true;
        p++;
        while (isdigit((unsigned char)*p)) p++;
    }
    if (*p == 'e' || *p == 'E') {
        const char *q = p + 1;
        if (*q == '+' || *q == '-') q++;
        if (!isdigit((unsigned char)*q)) return false;
        while (isdigit((unsigned char)*q)) q++;
        p = q;
        isf = true;
    }
    if (*p) return false;

    if (isf) { *out = mk_float(strtod(s, NULL)); return true; }

    /* Python não aceita zeros à esquerda em inteiros (007), mas aceita 00 */
    if (nd > 1 && d0[0] == '0')
        for (size_t i = 0; i < nd; i++)
            if (d0[i] != '0') return false;

    errno = 0;
    long long v = strtoll(s, NULL, 10);
    if (errno == ERANGE) return false;
    *out = mk_int(v);
    return true;
}

static bool eval_unary(const char *op, Val a, Val *out) {
    if (!strcmp(op, "not")) { *out = mk_bool(!truthy(a)); return true; }
    if (!strcmp(op, "+")) { *out = a.k == V_FLOAT ? a : mk_int(a.i); return true; }
    if (!strcmp(op, "-")) {
        if (a.k == V_FLOAT) { *out = mk_float(-a.d); return true; }
        if (a.i == LLONG_MIN) return false;
        *out = mk_int(-a.i);
        return true;
    }
    return false;
}

static bool eval_binary(Val a, const char *op, Val b, Val *out) {
    if (!strcmp(op, "and")) { *out = truthy(a) ? b : a; return true; }
    if (!strcmp(op, "or")) { *out = truthy(a) ? a : b; return true; }

    bool anyf = a.k == V_FLOAT || b.k == V_FLOAT;

    if (!strcmp(op, "<") || !strcmp(op, ">") || !strcmp(op, "<=") ||
        !strcmp(op, ">=") || !strcmp(op, "==") || !strcmp(op, "!=")) {
        int cmp;
        if (anyf) { double x = as_d(a), y = as_d(b); cmp = x < y ? -1 : x > y ? 1 : x == y ? 0 : 2; }
        else      { cmp = a.i < b.i ? -1 : a.i > b.i ? 1 : 0; }
        bool r;
        if (!strcmp(op, "<")) r = cmp == -1;
        else if (!strcmp(op, ">")) r = cmp == 1;
        else if (!strcmp(op, "<=")) r = cmp == -1 || cmp == 0;
        else if (!strcmp(op, ">=")) r = cmp == 1 || cmp == 0;
        else if (!strcmp(op, "==")) r = cmp == 0;
        else r = cmp != 0;
        *out = mk_bool(r);
        return true;
    }

    if (!strcmp(op, "/")) {
        if (as_d(b) == 0) return false; /* ZeroDivisionError */
        *out = mk_float(as_d(a) / as_d(b));
        return true;
    }

    if (!strcmp(op, "%")) {
        if (anyf) {
            return false; /* módulo de float não é dobrado (evita depender da libm) */
        } else {
            if (b.i == 0) return false;
            long long m = b.i == -1 ? 0 : a.i % b.i;
            if (m != 0 && ((m < 0) != (b.i < 0))) m += b.i;
            *out = mk_int(m);
        }
        return true;
    }

    if (!strcmp(op, "+") || !strcmp(op, "-") || !strcmp(op, "*")) {
        if (anyf) {
            double x = as_d(a), y = as_d(b);
            *out = mk_float(op[0] == '+' ? x + y : op[0] == '-' ? x - y : x * y);
        } else {
            long long r;
            bool ovf = op[0] == '+' ? __builtin_add_overflow(a.i, b.i, &r)
                     : op[0] == '-' ? __builtin_sub_overflow(a.i, b.i, &r)
                                    : __builtin_mul_overflow(a.i, b.i, &r);
            if (ovf) return false;
            *out = mk_int(r);
        }
        return true;
    }
    return false;
}

static bool eval_tokens(Vec *t, Val *out) {
    Val a, b;
    if (t->n == 1) return parse_val(t->v[0], out);
    if (t->n == 2) return parse_val(t->v[1], &a) && eval_unary(t->v[0], a, out);
    if (t->n == 3)
        return parse_val(t->v[0], &a) && parse_val(t->v[2], &b) && eval_binary(a, t->v[1], b, out);
    return false;
}

/* Equivalente ao str() do Python para float (repr mais curta que faz round-trip) */
static char *py_float_str(double d) {
    if (isnan(d)) return xstrdup("nan");
    if (isinf(d)) return xstrdup(d < 0 ? "-inf" : "inf");

    char buf[64];
    for (int prec = 1; prec <= 17; prec++) {
        snprintf(buf, sizeof buf, "%.*e", prec - 1, d);
        if (strtod(buf, NULL) == d) break;
    }

    bool neg = buf[0] == '-';
    const char *p = buf + (neg ? 1 : 0);
    char digs[32];
    int nd = 0;
    while (*p && *p != 'e') {
        if (*p != '.') digs[nd++] = *p;
        p++;
    }
    int exp10 = atoi(p + 1);
    while (nd > 1 && digs[nd - 1] == '0') nd--;
    digs[nd] = '\0';

    char out[128];
    int o = 0;
    if (neg) out[o++] = '-';

    if (exp10 >= -4 && exp10 < 16) {
        int decpt = exp10 + 1;
        if (decpt <= 0) {
            o += sprintf(out + o, "0.");
            for (int i = 0; i < -decpt; i++) out[o++] = '0';
            o += sprintf(out + o, "%s", digs);
        } else if (decpt >= nd) {
            o += sprintf(out + o, "%s", digs);
            for (int i = 0; i < decpt - nd; i++) out[o++] = '0';
            o += sprintf(out + o, ".0");
        } else {
            memcpy(out + o, digs, (size_t)decpt);
            o += decpt;
            out[o++] = '.';
            o += sprintf(out + o, "%s", digs + decpt);
        }
    } else {
        out[o++] = digs[0];
        if (nd > 1) o += sprintf(out + o, ".%s", digs + 1);
        o += sprintf(out + o, "e%c%02d", exp10 < 0 ? '-' : '+', abs(exp10));
    }
    out[o] = '\0';
    return xstrdup(out);
}

static char *val_str(Val v) {
    if (v.k == V_BOOL) return xstrdup(v.i ? "true" : "false");
    if (v.k == V_INT) return xfmt("%lld", v.i);
    return py_float_str(v.d);
}

static bool all_digits(const char *s) {
    if (!*s) return false;
    for (; *s; s++)
        if (!isdigit((unsigned char)*s)) return false;
    return true;
}

static void cdel(Vec *keys, Vec *vals, const char *name) {
    for (int k = 0; k < keys->n; k++)
        if (!strcmp(keys->v[k], name)) {
            keys->v[k] = keys->v[keys->n - 1];
            vals->v[k] = vals->v[vals->n - 1];
            keys->n--; vals->n--;
            return;
        }
}

static void run_opt(IRGen *G) {
    Vec opt = {0};
    Vec ckeys = {0}, cvals = {0}; /* dicionário de constantes: nome -> texto */

    for (int li = 0; li < G->code.n; li++) {
        char *line = G->code.v[li];
        size_t ll = strlen(line);

        /* rótulo ou início de função: outro fluxo pode chegar aqui, esquece tudo */
        if ((ll && line[ll - 1] == ':') || !strncmp(line, "func ", 5)) {
            ckeys.n = cvals.n = 0;
            vec_push(&opt, line);
            continue;
        }

        char *sep = strstr(line, " = ");
        if (!sep || strstr(line, "call") || strstr(line, "load") || strstr(line, "store")) {
            if (sep) { /* x = call/load ...: x deixa de ser constante */
                char *lhs = strip(xstrndup(line, (size_t)(sep - line)));
                cdel(&ckeys, &cvals, lhs);
            }
            vec_push(&opt, line);
            continue;
        }

        char *left = strip(xstrndup(line, (size_t)(sep - line)));
        char *right = strip(sep + 3);

        if (all_digits(right)) {
            int k;
            for (k = 0; k < ckeys.n; k++) if (!strcmp(ckeys.v[k], left)) break;
            if (k == ckeys.n) { vec_push(&ckeys, left); vec_push(&cvals, right); }
            else cvals.v[k] = right;
            vec_push(&opt, line);
            continue;
        }

        /* substitui cada token que seja uma constante conhecida */
        Vec parts = split_ws(right);
        for (int i = 0; i < parts.n; i++)
            for (int k = 0; k < ckeys.n; k++)
                if (!strcmp(ckeys.v[k], parts.v[i])) { parts.v[i] = cvals.v[k]; break; }

        Val v;
        if (eval_tokens(&parts, &v)) {
            char *s = val_str(v);
            int k;
            for (k = 0; k < ckeys.n; k++) if (!strcmp(ckeys.v[k], left)) break;
            if (k == ckeys.n) { vec_push(&ckeys, left); vec_push(&cvals, s); }
            else cvals.v[k] = s;
            vec_push(&opt, xfmt("  %s = %s", left, s));
        } else {
            cdel(&ckeys, &cvals, left); /* valor desconhecido: não é mais constante */
            char *joined = xstrdup("");
            for (int i = 0; i < parts.n; i++)
                joined = xfmt("%s%s%s", joined, i ? " " : "", (char *)parts.v[i]);
            vec_push(&opt, xfmt("  %s = %s", left, joined));
        }
    }
    G->code = opt;
}

static Vec generate_ir(Vec *tree, bool optimize) {
    IRGen G;
    memset(&G, 0, sizeof G);
    G.temp_cnt = 1;
    G.optimize = optimize;
    for (int i = 0; i < tree->n; i++) {
        Node *n = tree->v[i];
        if (n->k == N_FUNC) gen_func(&G, n);
    }
    if (G.optimize) run_opt(&G);
    return G.code;
}

/* ======================================================================
 * main
 * ====================================================================== */

static char *read_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    size_t cap = 4096, n = 0;
    char *buf = xmalloc(cap);
    size_t r;
    while ((r = fread(buf + n, 1, cap - n - 1, f)) > 0) {
        n += r;
        if (n + 1 >= cap) { cap *= 2; buf = xrealloc(buf, cap); }
    }
    buf[n] = '\0';
    fclose(f);
    return buf;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        puts("Uso: ./minic [-O0 | -O1] arquivo.c");
        return 2;
    }

    bool optimize = false;
    const char *path = NULL;
    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') { if (!strcmp(argv[i], "-O1")) optimize = true; }
        else if (!path) path = argv[i];
    }
    if (!path) return 2;

    char *src = read_file(path);
    if (!src) return 2;

    Parser p = {lex(src), 0};
    Vec tree = parse_program(&p);

    Vec errors = run_semantic_analysis(&tree);
    if (errors.n) {
        for (int i = 0; i < errors.n; i++) fprintf(stderr, "%s\n", (char *)errors.v[i]);
        return 4;
    }

    Vec ir = generate_ir(&tree, optimize);
    for (int i = 0; i < ir.n; i++) puts(ir.v[i]);
    return 0;
}
