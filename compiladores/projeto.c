/*
 ================================================================================
 Compilador PasKenzie - Fase 2 (Semântica + Geração de Código)
 Mantém a gramática da Fase 1 (program... var... begin... end).
 Gera MEPA no formato do exemplo do enunciado e usa hashMack() na Tabela de Símbolos.
 ================================================================================
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

/* ======================== Tokens e Estruturas ======================== */

typedef enum {
    // Palavras-chave
    PROGRAM, VAR, BEGIN, END, READ, WRITE,
    IF, THEN, ELSE, WHILE, DO,
    INTEGER, BOOLEAN, CHAR, NOT, AND, OR,
    TRUE, FALSE, DIV,

    // Literais/IDs
    IDENTIFIER, CONSTINT, CONSTCHAR,

    // Pontuação
    PONTO, PONTO_VIRGULA, VIRGULA, DOIS_PONTOS,
    ABRE_PAR, FECHA_PAR,

    // Operadores
    ATRIB, MENOR, MENOR_IGUAL, MAIOR, MAIOR_IGUAL,
    IGUAL, DIFERENTE, SOMA, SUB, MULT,

    // Especiais
    COMENTARIO, EOS, ERRO
} TAtomo;

typedef struct {
    TAtomo atomo;
    int linha;
    union {
        int  numero;    // CONSTINT
        char id[16];    // IDENTIFIER
        char ch;        // CONSTCHAR
    } atributo;
} TInfoAtomo;

/* ======================== Globais (Léxico) ======================== */

static char *entrada = NULL;
static char *cursor  = NULL;
static int linha_atual = 1;
static TInfoAtomo look;    // lookahead atual

/* ======================== Utilidades ======================== */

static const char* strAtomo(TAtomo t) {
    switch (t) {
        case PROGRAM: return "program";
        case VAR: return "var";
        case BEGIN: return "begin";
        case END: return "end";
        case READ: return "read";
        case WRITE: return "write";
        case IF: return "if";
        case THEN: return "then";
        case ELSE: return "else";
        case WHILE: return "while";
        case DO: return "do";
        case INTEGER: return "integer";
        case BOOLEAN: return "boolean";
        case CHAR: return "char";
        case NOT: return "not";
        case AND: return "and";
        case OR: return "or";
        case TRUE: return "true";
        case FALSE: return "false";
        case DIV: return "div";
        case IDENTIFIER: return "identifier";
        case CONSTINT: return "constint";
        case CONSTCHAR: return "constchar";
        case PONTO: return "ponto";
        case PONTO_VIRGULA: return "ponto_virgula";
        case VIRGULA: return "virgula";
        case DOIS_PONTOS: return "dois_pontos";
        case ABRE_PAR: return "abre_par";
        case FECHA_PAR: return "fecha_par";
        case ATRIB: return "atrib";
        case MENOR: return "menor";
        case MENOR_IGUAL: return "menor_igual";
        case MAIOR: return "maior";
        case MAIOR_IGUAL: return "maior_igual";
        case IGUAL: return "igual";
        case DIFERENTE: return "diferente";
        case SOMA: return "soma";
        case SUB: return "sub";
        case MULT: return "mult";
        case COMENTARIO: return "comentario";
        case EOS: return "eos";
        default: return "erro";
    }
}

static void erro_lexico(void) {
    printf("#   %d: erro lexico\n", linha_atual);
    exit(1);
}

static void erro_sintatico(TAtomo esperado) {
    printf("#   %d: erro sintatico, esperado [%s] encontrado [%s]\n",
           look.linha, strAtomo(esperado), strAtomo(look.atomo));
    exit(1);
}

static void erro_semantico(const char* msg, const char* id) {
    if (id)
        printf("Erro semantico na linha %d: %s '%s'.\n", linha_atual, msg, id);
    else
        printf("Erro semantico na linha %d: %s.\n", linha_atual, msg);
    exit(1);
}

/* ======================== Protótipos Sintático ======================== */

static void consome(TAtomo esperado);

static void parse_program(void);
static void parse_block(void);
static int  parse_variable_declaration_part(void); // retorna qtd de variáveis
static void parse_variable_declaration(void);
static void parse_type(void);
static void parse_statement_part(void);
static void parse_statement(void);
static void parse_assignment_statement(void);
static void parse_read_statement(void);
static void parse_write_statement(void);
static void parse_if_statement(void);
static void parse_while_statement(void);
static void parse_expression(void);
static void parse_simple_expression(void);
static void parse_term(void);
static void parse_factor(void);
static int  is_relational_or_logic_op(TAtomo t);
static void emit_compare_or_logic(TAtomo op);

/* ======================== Léxico ======================== */

static void skip_delims(void) {
    while (*cursor == ' ' || *cursor == '\t' || *cursor == '\r' || *cursor == '\n') {
        if (*cursor == '\n') linha_atual++;
        cursor++;
    }
}

static int is_identifier_start(char c) {
    return isalpha((unsigned char)c) || c == '_';
}

static int is_identifier_char(char c) {
    return isalnum((unsigned char)c) || c == '_';
}

static TInfoAtomo lex_comentario(void) {
    TInfoAtomo tk; tk.atomo = ERRO; tk.linha = linha_atual;
    cursor += 2; // pula "(*"
    while (*cursor) {
        if (*cursor == '\n') linha_atual++;
        if (*cursor == '*' && *(cursor+1) == ')') {
            cursor += 2;
            tk.atomo = COMENTARIO;
            tk.linha = linha_atual;
            return tk;
        }
        cursor++;
    }
    return tk; // não fechou: erro
}

static int reserved_lookup(const char* s) {
    if (strcmp(s, "program") == 0) return PROGRAM;
    if (strcmp(s, "var") == 0) return VAR;
    if (strcmp(s, "begin") == 0) return BEGIN;
    if (strcmp(s, "end") == 0) return END;
    if (strcmp(s, "read") == 0) return READ;
    if (strcmp(s, "write") == 0) return WRITE;
    if (strcmp(s, "if") == 0) return IF;
    if (strcmp(s, "then") == 0) return THEN;
    if (strcmp(s, "else") == 0) return ELSE;
    if (strcmp(s, "while") == 0) return WHILE;
    if (strcmp(s, "do") == 0) return DO;
    if (strcmp(s, "integer") == 0) return INTEGER;
    if (strcmp(s, "boolean") == 0) return BOOLEAN;
    if (strcmp(s, "char") == 0) return CHAR;
    if (strcmp(s, "not") == 0) return NOT;
    if (strcmp(s, "and") == 0) return AND;
    if (strcmp(s, "or") == 0) return OR;
    if (strcmp(s, "true") == 0) return TRUE;
    if (strcmp(s, "false") == 0) return FALSE;
    if (strcmp(s, "div") == 0) return DIV;
    return -1;
}

static TInfoAtomo lex_identifier_or_reserved(void) {
    TInfoAtomo tk; tk.atomo = ERRO; tk.linha = linha_atual;
    char buf[32]; int i = 0;
    while (is_identifier_char(*cursor)) {
        if (i < (int)sizeof(buf) - 1) buf[i++] = *cursor;
        cursor++;
    }
    buf[i] = '\0';
    int res = reserved_lookup(buf);
    if (res != -1) { tk.atomo = (TAtomo)res; return tk; }

    if ((int)strlen(buf) > 15) return tk; // erro: id > 15
    tk.atomo = IDENTIFIER;
    strncpy(tk.atributo.id, buf, sizeof(tk.atributo.id));
    tk.atributo.id[sizeof(tk.atributo.id)-1] = '\0';
    return tk;
}

static TInfoAtomo lex_constchar(void) {
    TInfoAtomo tk; tk.atomo = ERRO; tk.linha = linha_atual;
    cursor++; // '
    if (*cursor == '\0') return tk;
    tk.atributo.ch = *cursor;
    cursor++;
    if (*cursor != '\'') return tk;
    cursor++; // fecha '
    tk.atomo = CONSTCHAR;
    return tk;
}

static TInfoAtomo lex_constint(void) {
    TInfoAtomo tk; tk.atomo = ERRO; tk.linha = linha_atual;
    const char* ini = cursor;
    while (isdigit((unsigned char)*cursor)) cursor++;
    int has_digits = (cursor > ini);

    int exp_value = 0;
    if (*cursor == 'd') { // notação cientifica (ex: 12d3)
        cursor++;
        if (*cursor == '+') cursor++;
        const char* exp_ini = cursor;
        if (!isdigit((unsigned char)*cursor)) return tk;
        while (isdigit((unsigned char)*cursor)) cursor++;
        char expbuf[16]; int n = (int)(cursor - exp_ini);
        if (n >= (int)sizeof(expbuf)) n = (int)sizeof(expbuf)-1;
        strncpy(expbuf, exp_ini, n); expbuf[n] = '\0';
        exp_value = atoi(expbuf);
    }

    if (!has_digits) return tk;
    char nbuf[64]; int k = 0; const char* p = ini;
    while (p < cursor && *p != 'd' && k < (int)sizeof(nbuf)-1) nbuf[k++] = *p++;
    nbuf[k] = '\0';
    long base = atol(nbuf); long mul = 1;
    for (int i = 0; i < exp_value; i++) {
        if (mul > INT_MAX / 10) return tk;
        mul *= 10;
    }
    if (base > 0 && mul > 0 && base > INT_MAX / mul) return tk;

    tk.atomo = CONSTINT;
    tk.atributo.numero = (int)(base * mul);
    return tk;
}

static TInfoAtomo obter_atomo(void) {
    TInfoAtomo tk; tk.atomo = ERRO; tk.linha = linha_atual;
    skip_delims();
    if (*cursor == '\0') { tk.atomo = EOS; tk.linha = linha_atual; return tk; }

    if (*cursor == '(' && *(cursor+1) == '*') return lex_comentario();
    if (*cursor == ';') { cursor++; tk.atomo = PONTO_VIRGULA; tk.linha = linha_atual; return tk; }
    if (*cursor == ',') { cursor++; tk.atomo = VIRGULA; tk.linha = linha_atual; return tk; }
    if (*cursor == '.') { cursor++; tk.atomo = PONTO; tk.linha = linha_atual; return tk; }
    if (*cursor == '(') { cursor++; tk.atomo = ABRE_PAR; tk.linha = linha_atual; return tk; }
    if (*cursor == ')') { cursor++; tk.atomo = FECHA_PAR; tk.linha = linha_atual; return tk; }
    if (*cursor == ':') {
        cursor++;
        if (*cursor == '=') { cursor++; tk.atomo = ATRIB; tk.linha = linha_atual; return tk; }
        tk.atomo = DOIS_PONTOS; tk.linha = linha_atual; return tk;
    }
    if (*cursor == '+') { cursor++; tk.atomo = SOMA; tk.linha = linha_atual; return tk; }
    if (*cursor == '-') { cursor++; tk.atomo = SUB;  tk.linha = linha_atual; return tk; }
    if (*cursor == '*') { cursor++; tk.atomo = MULT; tk.linha = linha_atual; return tk; }
    if (*cursor == '<') {
        cursor++;
        if (*cursor == '=') { cursor++; tk.atomo = MENOR_IGUAL; }
        else if (*cursor == '>') { cursor++; tk.atomo = DIFERENTE; }
        else tk.atomo = MENOR;
        tk.linha = linha_atual; return tk;
    }
    if (*cursor == '>') {
        cursor++;
        if (*cursor == '=') { cursor++; tk.atomo = MAIOR_IGUAL; }
        else tk.atomo = MAIOR;
        tk.linha = linha_atual; return tk;
    }
    if (*cursor == '=') { cursor++; tk.atomo = IGUAL; tk.linha = linha_atual; return tk; }

    if (*cursor == '\'') return lex_constchar();
    if (isdigit((unsigned char)*cursor)) return lex_constint();
    if (is_identifier_start(*cursor)) return lex_identifier_or_reserved();

    return tk;
}

/* ======================== Tabela de Símbolos (Hash) ======================== */

#define PRIME_NUMER 211

typedef struct _TNo {
    char ID[16];
    int  endereco;
    struct _TNo *prox;
} TNo;

typedef struct {
    TNo *entradas[PRIME_NUMER];
} TTabelaSimbolos;

static TTabelaSimbolos TS;
static int endereco_atual = 0; // 0,1,2,... na ordem de declaração

// fornecido em hashMack.o / hashMack.obj
int hashMack(char *s);

static void ts_init(void) {
    for (int i = 0; i < PRIME_NUMER; i++) TS.entradas[i] = NULL;
}

static void ts_add(char *id) {
    int idx = hashMack(id) % PRIME_NUMER;
    for (TNo *p = TS.entradas[idx]; p; p = p->prox) {
        if (strcmp(p->ID, id) == 0) {
            erro_semantico("variavel redeclarada", id);
        }
    }
    TNo *novo = (TNo*)malloc(sizeof(TNo));
    if (!novo) { perror("malloc"); exit(1); }
    strncpy(novo->ID, id, 16); novo->ID[15] = '\0';
    novo->endereco = endereco_atual++;
    novo->prox = TS.entradas[idx];
    TS.entradas[idx] = novo;
}

static int ts_busca_end(char *id) {
    int idx = hashMack(id) % PRIME_NUMER;
    for (TNo *p = TS.entradas[idx]; p; p = p->prox) {
        if (strcmp(p->ID, id) == 0) return p->endereco;
    }
    erro_semantico("variavel nao declarada", id);
    return -1;
}

static void ts_free(void) {
    for (int i = 0; i < PRIME_NUMER; i++) {
        TNo *p = TS.entradas[i];
        while (p) { TNo *n = p->prox; free(p); p = n; }
        TS.entradas[i] = NULL;
    }
}

static void ts_print(void) {
    printf("\nTABELA DE SIMBOLOS\n");
    for (int i = 0; i < PRIME_NUMER; i++) {
        for (TNo *p = TS.entradas[i]; p; p = p->prox) {
            printf("Entrada Tabela Simbolos: [%d] => %s | Endereco: %d\n", i, p->ID, p->endereco);
        }
    }
}

/* ======================== Emissão de Código MEPA ======================== */

static int rotulo_atual = 0;
static int proximo_rotulo(void) { return ++rotulo_atual; }

static void emit(const char *s) { printf("%s\n", s); }
static void emit1(const char *fmt, int a) { printf(fmt, a); putchar('\n'); }
static void emit_lab(int L) { printf("L%d:\tNADA\n", L); }

/* ======================== Parser + Codegen ======================== */

static void avancar_lex(void) {
    look = obter_atomo();
    if (look.atomo == ERRO) erro_lexico();
}

static void consome(TAtomo esperado) {
    // pular comentários automaticamente
    while (look.atomo == COMENTARIO) avancar_lex();

    if (look.atomo == esperado) {
        avancar_lex();
        // pular comentários subsequentes
        while (look.atomo == COMENTARIO) avancar_lex();
    } else {
        erro_sintatico(esperado);
    }
}

/* program -> 'program' identifier ';' block '.' */
static void parse_program(void) {
    ts_init();
    consome(PROGRAM);
    consome(IDENTIFIER);
    consome(PONTO_VIRGULA);

    emit("\tINPP");  // início do programa
    parse_block();
    consome(PONTO);

    emit("\tPARA");  // fim do programa
}

/* block -> variable_declaration_part statement_part */
static void parse_block(void) {
    int qtd_vars = parse_variable_declaration_part(); // conta quantas variáveis
    if (qtd_vars > 0) emit1("\tAMEM %d", qtd_vars);   // aloca memória na pilha
    parse_statement_part();
}

/* variable_declaration_part -> [ 'var' (variable_declaration ';')+ ]
   Retorna a quantidade de variáveis declaradas (para AMEM). */
static int parse_variable_declaration_part(void) {
    int inicio_end = endereco_atual;
    if (look.atomo == VAR) {
        consome(VAR);
        parse_variable_declaration();
        consome(PONTO_VIRGULA);
        while (look.atomo == IDENTIFIER) {
            parse_variable_declaration();
            consome(PONTO_VIRGULA);
        }
    }
    return endereco_atual - inicio_end;
}

/* variable_declaration -> identifier (',' identifier)* ':' type */
static void parse_variable_declaration(void) {
    // primeira id
    if (look.atomo != IDENTIFIER) erro_sintatico(IDENTIFIER);
    ts_add(look.atributo.id);
    consome(IDENTIFIER);

    while (look.atomo == VIRGULA) {
        consome(VIRGULA);
        if (look.atomo != IDENTIFIER) erro_sintatico(IDENTIFIER);
        ts_add(look.atributo.id);
        consome(IDENTIFIER);
    }
    consome(DOIS_PONTOS);
    parse_type(); // types aceitos, mas não diferenciamos no MEPA (simplificação)
}

/* type -> 'integer' | 'char' | 'boolean' */
static void parse_type(void) {
    if (look.atomo == CHAR) consome(CHAR);
    else if (look.atomo == INTEGER) consome(INTEGER);
    else if (look.atomo == BOOLEAN) consome(BOOLEAN);
    else erro_sintatico(INTEGER);
}

/* statement_part -> 'begin' statement (';' statement)* 'end' */
static void parse_statement_part(void) {
    consome(BEGIN);
    parse_statement();
    while (look.atomo == PONTO_VIRGULA || look.atomo == COMENTARIO) {
        if (look.atomo == PONTO_VIRGULA) consome(PONTO_VIRGULA);
        while (look.atomo == COMENTARIO) consome(COMENTARIO);
        if (look.atomo == END) break;
        parse_statement();
    }
    consome(END);
}

/* statement */
static void parse_statement(void) {
    switch (look.atomo) {
        case IDENTIFIER: parse_assignment_statement(); break;
        case READ:       parse_read_statement();       break;
        case WRITE:      parse_write_statement();      break;
        case IF:         parse_if_statement();         break;
        case WHILE:      parse_while_statement();      break;
        case BEGIN:      parse_statement_part();       break;
        default:         erro_sintatico(IDENTIFIER);
    }
}

/* assignment_statement -> identifier ':=' expression */
static void parse_assignment_statement(void) {
    if (look.atomo != IDENTIFIER) erro_sintatico(IDENTIFIER);
    int end = ts_busca_end(look.atributo.id);
    consome(IDENTIFIER);
    consome(ATRIB);
    parse_expression();          // expressão empilha resultado no topo
    // ARMZ end
    char buf[64]; snprintf(buf, sizeof(buf), "\tARMZ %d", end);
    emit(buf);
}

/* read_statement -> 'read' '(' identifier (',' identifier)* ')' */
static void parse_read_statement(void) {
    consome(READ);
    consome(ABRE_PAR);
    if (look.atomo != IDENTIFIER) erro_sintatico(IDENTIFIER);
    // para cada id: LEIT ; ARMZ end
    do {
        int end = ts_busca_end(look.atributo.id);
        consome(IDENTIFIER);
        emit("\tLEIT");
        char buf[64]; snprintf(buf, sizeof(buf), "\tARMZ %d", end);
        emit(buf);
        if (look.atomo == VIRGULA) consome(VIRGULA); else break;
    } while (1);
    consome(FECHA_PAR);
}

/* write_statement -> 'write' '(' identifier (',' identifier)* ')' */
static void parse_write_statement(void) {
    consome(WRITE);
    consome(ABRE_PAR);
    if (look.atomo != IDENTIFIER) erro_sintatico(IDENTIFIER);
    // para cada id: CRVL end ; IMPR
    do {
        int end = ts_busca_end(look.atributo.id);
        consome(IDENTIFIER);
        char buf[64]; snprintf(buf, sizeof(buf), "\tCRVL %d", end);
        emit(buf);
        emit("\tIMPR");
        if (look.atomo == VIRGULA) consome(VIRGULA); else break;
    } while (1);
    consome(FECHA_PAR);
}

/* if_statement -> 'if' expression 'then' statement ['else' statement] */
static void parse_if_statement(void) {
    consome(IF);
    parse_expression(); // topo da pilha = resultado (1/0)
    consome(THEN);
    int L1 = proximo_rotulo();
    int L2 = proximo_rotulo();
    char buf[64];
    snprintf(buf, sizeof(buf), "\tDSVF L%d", L1); emit(buf);
    parse_statement();
    snprintf(buf, sizeof(buf), "\tDSVS L%d", L2); emit(buf);
    emit_lab(L1);
    if (look.atomo == ELSE) {
        consome(ELSE);
        parse_statement();
    }
    emit_lab(L2);
}

/* while_statement -> 'while' expression 'do' statement */
static void parse_while_statement(void) {
    consome(WHILE);
    int L1 = proximo_rotulo();
    int L2 = proximo_rotulo();
    emit_lab(L1);
    parse_expression();                    // condição
    char buf[64];
    snprintf(buf, sizeof(buf), "\tDSVF L%d", L2); emit(buf);
    consome(DO);
    parse_statement();
    snprintf(buf, sizeof(buf), "\tDSVS L%d", L1); emit(buf);
    emit_lab(L2);
}

/* expression -> simple_expression [ (rel_or_logic_op) simple_expression ] */
static void parse_expression(void) {
    parse_simple_expression();
    if (is_relational_or_logic_op(look.atomo)) {
        TAtomo op = look.atomo;
        consome(look.atomo);
        parse_simple_expression();
        emit_compare_or_logic(op);
    }
}

/* simple_expression -> term (('+'|'-'|'or'|'and') term)* 
   (permitimos 'or'/'and' aqui também para maior flexibilidade) */
static void parse_simple_expression(void) {
    parse_term();
    while (look.atomo == SOMA || look.atomo == SUB || look.atomo == OR || look.atomo == AND) {
        TAtomo op = look.atomo;
        consome(op);
        parse_term();
        if (op == SOMA) emit("\tSOMA");
        else if (op == SUB) emit("\tSUBT");
        else if (op == OR) emit("\tDISJ");
        else emit("\tCONJ"); // AND
    }
}

/* term -> factor (('*' | 'div') factor)* */
static void parse_term(void) {
    parse_factor();
    while (look.atomo == MULT || look.atomo == DIV) {
        TAtomo op = look.atomo;
        consome(op);
        parse_factor();
        if (op == MULT) emit("\tMULT");
        else emit("\tDIVI"); // div
    }
}

/* factor -> identifier | constint | constchar | true | false | '(' expression ')' | 'not' factor */
static void parse_factor(void) {
    switch (look.atomo) {
        case IDENTIFIER: {
            int end = ts_busca_end(look.atributo.id);
            char buf[64]; snprintf(buf, sizeof(buf), "\tCRVL %d", end);
            emit(buf);
            consome(IDENTIFIER);
        } break;
        case CONSTINT:
            emit1("\tCRCT %d", look.atributo.numero);
            consome(CONSTINT);
            break;
        case CONSTCHAR:
            emit1("\tCRCT %d", (int)look.atributo.ch); // empilha código ASCII
            consome(CONSTCHAR);
            break;
        case TRUE:
            emit("\tCRCT 1"); consome(TRUE); break;
        case FALSE:
            emit("\tCRCT 0"); consome(FALSE); break;
        case ABRE_PAR:
            consome(ABRE_PAR);
            parse_expression();
            consome(FECHA_PAR);
            break;
        case NOT:
            consome(NOT);
            parse_factor();
            emit("\tNEGA");
            break;
        default:
            erro_sintatico(IDENTIFIER);
    }
}

static int is_relational_or_logic_op(TAtomo t) {
    return t == IGUAL || t == DIFERENTE ||
           t == MENOR || t == MENOR_IGUAL ||
           t == MAIOR || t == MAIOR_IGUAL ||
           t == OR    || t == AND;
}

static void emit_compare_or_logic(TAtomo op) {
    switch (op) {
        case IGUAL:        emit("\tCMIG"); break;
        case DIFERENTE:    emit("\tCMDG"); break;
        case MENOR:        emit("\tCMME"); break;
        case MENOR_IGUAL:  emit("\tCMEG"); break;
        case MAIOR:        emit("\tCMMA"); break;
        case MAIOR_IGUAL:  emit("\tCMAG"); break;
        case OR:           emit("\tDISJ"); break;
        case AND:          emit("\tCONJ"); break;
        default: break;
    }
}

/* ======================== Main ======================== */

int main(int argc, char** argv) {
    if (argc < 2) {
        printf("\nUso: %s <arquivo_fonte>\n\n", argv[0]);
        return 1;
    }
    FILE* f = fopen(argv[1], "rb");
    if (!f) { perror("Erro ao abrir arquivo"); return 1; }

    fseek(f, 0, SEEK_END);
    long tam = ftell(f);
    fseek(f, 0, SEEK_SET);

    entrada = (char*)calloc((size_t)tam + 1, 1);
    if (!entrada) { fclose(f); fprintf(stderr, "Falha ao alocar memoria\n"); return 1; }
    fread(entrada, 1, (size_t)tam, f);
    fclose(f);

    cursor = entrada;
    avancar_lex();       // pega primeiro token
    parse_program();     // analisa + gera código

    if (look.atomo != EOS) erro_sintatico(EOS);

    // imprime tabela de símbolos
    ts_print();

    // limpeza
    ts_free();
    free(entrada);
    return 0;
}
