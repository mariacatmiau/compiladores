/*
 ================================================================================
 Compilador PasKenzie - Fase 2
 Nome: Maria Gabriela 10409037
 Nome: Raphaela Polonis 10408843
 
 1. Implementar a Tabela de Símbolos (HashTable) conforme especificado.
 2. Realizar a Análise Semântica (verificar duplicatas e uso de vars).
 3. Gerar Código Intermediário MEPA durante a análise sintática.
 ================================================================================
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

/* ======================== 1. Tokens e Estruturas ======================== */

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

// Estrutura para guardar a informação de cada token
typedef struct {
    TAtomo atomo; // O tipo do token (ex: IDENTIFIER)
    int linha;    // Linha atual (para mensagens de erro)
    union {
        int  numero;   // se for CONSTINT
        char id[16];   // se for IDENTIFIER (limite de 15 chars + \0)
        char ch;       // se for CONSTCHAR
    } atributo;
} TInfoAtomo;

/* ======================== 2. Globais do Compilador ======================== */

static char *entrada = NULL; // Ponteiro para o buffer com o código-fonte
static char *cursor  = NULL; // Onde estamos lendo no buffer
static int linha_atual = 1;  // Contador de linhas
static TInfoAtomo look;      // O token atual que estamos analisando (lookahead)

/* ======================== 3. Funções Utilitárias (Erros, etc) ======================== */

// Apenas para debug e mensagens de erro (converte enum em string)
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

// Funções de erro: Imprimem a mensagem e param a compilação (exit(1))

static void erro_lexico(void) {
    printf("#   %d: erro lexico\n", linha_atual);
    exit(1);
}

static void erro_sintatico(TAtomo esperado) {
    printf("#   %d: erro sintatico, esperado [%s] encontrado [%s]\n",
           look.linha, strAtomo(esperado), strAtomo(look.atomo));
    exit(1);
}

// Erro da FASE 2: Imprime o erro semântico e para
static void erro_semantico(const char* msg, const char* id) {
    if (id)
        printf("Erro semantico na linha %d: %s '%s'.\n", linha_atual, msg, id);
    else
        printf("Erro semantico na linha %d: %s.\n", linha_atual, msg);
    exit(1);
}

/* ======================== 4. Protótipos das Funções do Parser ======================== */

// (Declarações 'forward' para as funções de análise sintática que se chamam)

static void consome(TAtomo esperado);
static void parse_program(void);
static void parse_block(void);
static int  parse_variable_declaration_part(void); // Retorna o total de vars para o AMEM
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

/* ======================== 5. Analisador Léxico (Fase 1) ======================== */

// Pula espaço em branco, tabs e newlines
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

// Trata comentários (* ... *)
static TInfoAtomo lex_comentario(void) {
    TInfoAtomo tk; tk.atomo = ERRO; tk.linha = linha_atual;
    cursor += 2; // pula "(*"
    while (*cursor) {
        if (*cursor == '\n') linha_atual++;
        if (*cursor == '*' && *(cursor+1) == ')') {
            cursor += 2; // pula "*)
            tk.atomo = COMENTARIO;
            tk.linha = linha_atual;
            return tk;
        }
        cursor++;
    }
    return tk; // se chegou ao fim do arquivo (EOS) e não fechou, é erro
}

// Verifica se uma string é uma palavra-chave
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
    return -1; // não é palavra reservada
}

// Trata IDs e Palavras-Chave
static TInfoAtomo lex_identifier_or_reserved(void) {
    TInfoAtomo tk; tk.atomo = ERRO; tk.linha = linha_atual;
    char buf[32]; int i = 0;
    while (is_identifier_char(*cursor)) {
        if (i < (int)sizeof(buf) - 1) buf[i++] = *cursor;
        cursor++;
    }
    buf[i] = '\0';
    
    // 1. É palavra reservada?
    int res = reserved_lookup(buf);
    if (res != -1) { tk.atomo = (TAtomo)res; return tk; }

    // 2. Senão, é um IDENTIFIER
    // (O enunciado limita o ID em 15 chars)
    if ((int)strlen(buf) > 15) return tk; // erro: id > 15
    
    tk.atomo = IDENTIFIER;
    strncpy(tk.atributo.id, buf, sizeof(tk.atributo.id));
    tk.atributo.id[sizeof(tk.atributo.id)-1] = '\0';
    return tk;
}

// Trata 'a', 'b', etc.
static TInfoAtomo lex_constchar(void) {
    TInfoAtomo tk; tk.atomo = ERRO; tk.linha = linha_atual;
    cursor++; // pula '
    if (*cursor == '\0') return tk;
    tk.atributo.ch = *cursor;
    cursor++;
    if (*cursor != '\'') return tk; // erro, não fechou aspas
    cursor++; // pula '
    tk.atomo = CONSTCHAR;
    return tk;
}

// Trata números, ex: 123, e também a notação 12d3 (12000)
static TInfoAtomo lex_constint(void) {
    TInfoAtomo tk; tk.atomo = ERRO; tk.linha = linha_atual;
    const char* ini = cursor;
    while (isdigit((unsigned char)*cursor)) cursor++;
    int has_digits = (cursor > ini);

    // Suporte à notação cientifica 'd' (ex: 12d3)
    int exp_value = 0;
    if (*cursor == 'd') {
        cursor++;
        if (*cursor == '+') cursor++;
        const char* exp_ini = cursor;
        if (!isdigit((unsigned char)*cursor)) return tk; // 'd' sem número
        while (isdigit((unsigned char)*cursor)) cursor++;
        char expbuf[16]; int n = (int)(cursor - exp_ini);
        if (n >= (int)sizeof(expbuf)) n = (int)sizeof(expbuf)-1;
        strncpy(expbuf, exp_ini, n); expbuf[n] = '\0';
        exp_value = atoi(expbuf);
    }

    if (!has_digits) return tk; // 'd' sozinho?
    
    // Pega a parte base do número
    char nbuf[64]; int k = 0; const char* p = ini;
    while (p < cursor && *p != 'd' && k < (int)sizeof(nbuf)-1) nbuf[k++] = *p++;
    nbuf[k] = '\0';
    
    // Calcula o valor final (base * 10^exp)
    long base = atol(nbuf); long mul = 1;
    for (int i = 0; i < exp_value; i++) {
        if (mul > INT_MAX / 10) return tk; // overflow
        mul *= 10;
    }
    if (base > 0 && mul > 0 && base > INT_MAX / mul) return tk; // overflow

    tk.atomo = CONSTINT;
    tk.atributo.numero = (int)(base * mul);
    return tk;
}

// Função principal do Léxico: "O próximo token, por favor"
static TInfoAtomo obter_atomo(void) {
    TInfoAtomo tk; tk.atomo = ERRO; tk.linha = linha_atual;
    skip_delims();
    if (*cursor == '\0') { tk.atomo = EOS; tk.linha = linha_atual; return tk; }

    // Comentário?
    if (*cursor == '(' && *(cursor+1) == '*') return lex_comentario();
    
    // Pontuação simples
    if (*cursor == ';') { cursor++; tk.atomo = PONTO_VIRGULA; tk.linha = linha_atual; return tk; }
    if (*cursor == ',') { cursor++; tk.atomo = VIRGULA; tk.linha = linha_atual; return tk; }
    if (*cursor == '.') { cursor++; tk.atomo = PONTO; tk.linha = linha_atual; return tk; }
    if (*cursor == '(') { cursor++; tk.atomo = ABRE_PAR; tk.linha = linha_atual; return tk; }
    if (*cursor == ')') { cursor++; tk.atomo = FECHA_PAR; tk.linha = linha_atual; return tk; }
    
    // Pontuação/Ops (podem ter 2 chars)
    if (*cursor == ':') {
        cursor++;
        if (*cursor == '=') { cursor++; tk.atomo = ATRIB; tk.linha = linha_atual; return tk; } // :=
        tk.atomo = DOIS_PONTOS; tk.linha = linha_atual; return tk; // :
    }
    if (*cursor == '+') { cursor++; tk.atomo = SOMA; tk.linha = linha_atual; return tk; }
    if (*cursor == '-') { cursor++; tk.atomo = SUB;  tk.linha = linha_atual; return tk; }
    if (*cursor == '*') { cursor++; tk.atomo = MULT; tk.linha = linha_atual; return tk; }
    if (*cursor == '<') {
        cursor++;
        if (*cursor == '=') { cursor++; tk.atomo = MENOR_IGUAL; } // <=
        else if (*cursor == '>') { cursor++; tk.atomo = DIFERENTE; } // <>
        else tk.atomo = MENOR; // <
        tk.linha = linha_atual; return tk;
    }
    if (*cursor == '>') {
        cursor++;
        if (*cursor == '=') { cursor++; tk.atomo = MAIOR_IGUAL; } // >=
        else tk.atomo = MAIOR; // >
        tk.linha = linha_atual; return tk;
    }
    if (*cursor == '=') { cursor++; tk.atomo = IGUAL; tk.linha = linha_atual; return tk; }

    // Literais
    if (*cursor == '\'') return lex_constchar();
    if (isdigit((unsigned char)*cursor)) return lex_constint();
    if (is_identifier_start(*cursor)) return lex_identifier_or_reserved();

    return tk; // Se chegou aqui, não reconheceu nada
}

/* ======================== 6. Tabela de Símbolos (FASE 2) ======================== */

// (Definições obrigatórias do enunciado)
#define PRIME_NUMER 211

typedef struct _TNo {
    char ID[16];
    int  endereco; // Endereço MEPA (0, 1, 2...)
    struct _TNo *prox;
} TNo;

typedef struct {
    TNo *entradas[PRIME_NUMER];
} TTabelaSimbolos;

// Globais da Tabela de Símbolos
static TTabelaSimbolos TS;
static int endereco_atual = 0; // Contador para o próximo endereço vago (0, 1, 2...)

// Função externa, fornecida em hashMack.o / hashMack.obj
int hashMack(char *s);

// Zera a tabela (coloca NULL em todas as entradas)
static void ts_init(void) {
    for (int i = 0; i < PRIME_NUMER; i++) TS.entradas[i] = NULL;
}

// Adiciona um ID na Tabela (na seção 'var')
static void ts_add(char *id) {
    int idx = hashMack(id) % PRIME_NUMER;
    
    // ANÁLISE SEMÂNTICA (Declaração):
    // Antes de adicionar, verifica se o ID já existe *nessa* lista (bucket)
    for (TNo *p = TS.entradas[idx]; p; p = p->prox) {
        if (strcmp(p->ID, id) == 0) {
            // Se achou, é um erro!
            erro_semantico("variavel redeclarada", id);
        }
    }
    
    // Tudo certo, pode adicionar.
    TNo *novo = (TNo*)malloc(sizeof(TNo));
    if (!novo) { perror("malloc"); exit(1); }
    
    strncpy(novo->ID, id, 16); novo->ID[15] = '\0';
    novo->endereco = endereco_atual++; // Pega o endereço atual e incrementa
    novo->prox = TS.entradas[idx];     // Insere no início da lista
    TS.entradas[idx] = novo;
}

// Busca um ID na Tabela (no 'begin...end.') e retorna seu endereço
static int ts_busca_end(char *id) {
    int idx = hashMack(id) % PRIME_NUMER;
    
    // ANÁLISE SEMÂNTICA (Uso):
    // Procura o ID na lista correspondente
    for (TNo *p = TS.entradas[idx]; p; p = p->prox) {
        if (strcmp(p->ID, id) == 0) {
            return p->endereco; // Achou! Retorna o endereço MEPA.
        }
    }
    
    // Se saiu do loop e não achou...
    // Erro! Variável não declarada.
    erro_semantico("variavel nao declarada", id);
    return -1; // (nunca chega aqui, pois o erro_semantico() dá exit)
}

// Libera a memória alocada para a Tabela
static void ts_free(void) {
    for (int i = 0; i < PRIME_NUMER; i++) {
        TNo *p = TS.entradas[i];
        while (p) {
            TNo *n = p->prox;
            free(p);
            p = n;
        }
        TS.entradas[i] = NULL;
    }
}

// Imprime a Tabela no formato exigido
static void ts_print(void) {
    printf("\nTABELA DE SIMBOLOS\n");
    for (int i = 0; i < PRIME_NUMER; i++) {
        for (TNo *p = TS.entradas[i]; p; p = p->prox) {
            printf("Entrada Tabela Simbolos: [%d] => %s | Endereco: %d\n", i, p->ID, p->endereco);
        }
    }
}

/* ======================== 7. Emissão de Código MEPA (FASE 2) ======================== */

static int rotulo_atual = 0; // Contador para gerar rótulos (L1, L2, L3...)
static int proximo_rotulo(void) { return ++rotulo_atual; }

// Funções helpers para 'printar' o código MEPA
static void emit(const char *s) { printf("%s\n", s); } // ex: emit("\tSOMA")
static void emit1(const char *fmt, int a) { printf(fmt, a); putchar('\n'); } // ex: emit1("\tCRCT %d", 5)
static void emit_lab(int L) { printf("L%d:\tNADA\n", L); } // ex: emit_lab(1) -> "L1: NADA"

/* ======================== 8. Parser + Gerador de Código ======================== */

// Pega o próximo token (e ignora se for comentário)
static void avancar_lex(void) {
    look = obter_atomo();
    if (look.atomo == ERRO) erro_lexico();
}

// 'Casa' o token atual com o esperado e avança
static void consome(TAtomo esperado) {
    // Essa lógica é importante: pula comentários automaticamente
    while (look.atomo == COMENTARIO) avancar_lex();

    if (look.atomo == esperado) {
        avancar_lex();
        // Pula comentários *depois* do token (ex: 'begin (* oi *) ...')
        while (look.atomo == COMENTARIO) avancar_lex();
    } else {
        erro_sintatico(esperado);
    }
}

/* program -> 'program' identifier ';' block '.' */
static void parse_program(void) {
    ts_init(); // Inicializa a Tabela de Símbolos
    consome(PROGRAM);
    consome(IDENTIFIER);
    consome(PONTO_VIRGULA);

    emit("\tINPP"); // Geração de Código MEPA: Início
    parse_block();
    consome(PONTO);
    emit("\tPARA"); // Geração de Código MEPA: Fim
}

/* block -> variable_declaration_part statement_part */
static void parse_block(void) {
    int qtd_vars = parse_variable_declaration_part(); // Pega o total de vars
    if (qtd_vars > 0) {
        emit1("\tAMEM %d", qtd_vars); // Aloca memória na pilha MEPA
    }
    parse_statement_part();
}

/* variable_declaration_part -> [ 'var' (variable_declaration ';')+ ]
   Retorna a quantidade de variáveis declaradas (para o AMEM). */
static int parse_variable_declaration_part(void) {
    int inicio_end = endereco_atual; // Salva o endereço 'antes' de começar
    if (look.atomo == VAR) {
        consome(VAR);
        parse_variable_declaration();
        consome(PONTO_VIRGULA);
        while (look.atomo == IDENTIFIER) { // Loop para múltiplas declarações
            parse_variable_declaration();
            consome(PONTO_VIRGULA);
        }
    }
    // Retorna quantas vars foram de fato adicionadas
    return endereco_atual - inicio_end;
}

/* variable_declaration -> identifier (',' identifier)* ':' type */
static void parse_variable_declaration(void) {
    // primeiro id
    if (look.atomo != IDENTIFIER) erro_sintatico(IDENTIFIER);
    ts_add(look.atributo.id); // Adiciona na Tabela (e checa duplicata)
    consome(IDENTIFIER);

    // Loop para 'a, b, c : ...'
    while (look.atomo == VIRGULA) {
        consome(VIRGULA);
        if (look.atomo != IDENTIFIER) erro_sintatico(IDENTIFIER);
        ts_add(look.atributo.id); // Adiciona na Tabela (e checa duplicata)
        consome(IDENTIFIER);
    }
    consome(DOIS_PONTOS);
    
    // A gente 'consome' o tipo, mas ignora (simplificação do enunciado)
    parse_type();
}

/* type -> 'integer' | 'char' | 'boolean' */
static void parse_type(void) {
    if (look.atomo == CHAR) consome(CHAR);
    else if (look.atomo == INTEGER) consome(INTEGER);
    else if (look.atomo == BOOLEAN) consome(BOOLEAN);
    else erro_sintatico(INTEGER); // Esperava um tipo
}

/* statement_part -> 'begin' statement (';' statement)* 'end' */
static void parse_statement_part(void) {
    consome(BEGIN);
    parse_statement();
    // Loop para vários comandos separados por ';'
    while (look.atomo == PONTO_VIRGULA || look.atomo == COMENTARIO) {
        if (look.atomo == PONTO_VIRGULA) consome(PONTO_VIRGULA);
        while (look.atomo == COMENTARIO) consome(COMENTARIO); // ignora
        
        // Cuidado: 'begin ... end' é válido, não pode ter ';' antes do 'end'
        if (look.atomo == END) break; 
        
        parse_statement();
    }
    consome(END);
}

/* statement -> (várias opções) */
static void parse_statement(void) {
    switch (look.atomo) {
        case IDENTIFIER: parse_assignment_statement(); break;
        case READ:       parse_read_statement();       break;
        case WRITE:      parse_write_statement();      break;
        case IF:         parse_if_statement();         break;
        case WHILE:      parse_while_statement();      break;
        case BEGIN:      parse_statement_part();       break;
        default:         // Se não for nada disso, é um erro
            erro_sintatico(IDENTIFIER);
    }
}

/* assignment_statement -> identifier ':=' expression */
static void parse_assignment_statement(void) {
    if (look.atomo != IDENTIFIER) erro_sintatico(IDENTIFIER);
    
    // 1. Pega o endereço da var (e checa se ela foi declarada)
    int end = ts_busca_end(look.atributo.id);
    consome(IDENTIFIER);
    consome(ATRIB);
    
    // 2. Gera código para a expressão (o resultado vai para o topo da pilha)
    parse_expression();
    
    // 3. ARMZ: Armazena o resultado (topo da pilha) no endereço da variável
    char buf[64]; snprintf(buf, sizeof(buf), "\tARMZ %d", end);
    emit(buf);
}

/* read_statement -> 'read' '(' identifier (',' identifier)* ')' */
static void parse_read_statement(void) {
    consome(READ);
    consome(ABRE_PAR);
    if (look.atomo != IDENTIFIER) erro_sintatico(IDENTIFIER);
    
    // Loop para read(a, b, ...)
    do {
        // Para cada ID:
        // 1. Checa se foi declarado e pega o endereço
        int end = ts_busca_end(look.atributo.id);
        consome(IDENTIFIER);
        
        // 2. Gera LEIT (lê) e ARMZ (armazena no endereço)
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
    
    // Loop para write(a, b, ...)
    do {
        // Para cada ID:
        // 1. Checa se foi declarado e pega o endereço
        int end = ts_busca_end(look.atributo.id);
        consome(IDENTIFIER);
        
        // 2. Gera CRVL (carrega valor do endereço) e IMPR (imprime)
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
    parse_expression(); // Gera código da condição (resultado 0 ou 1 no topo)
    consome(THEN);
    
    // Pega dois rótulos: L1 (para o 'else') e L2 (para o 'fim-do-if')
    int L1 = proximo_rotulo();
    int L2 = proximo_rotulo();
    
    // DSVF L1: "Desvie Se Falso". Se (condição) == 0, pula pro ELSE (L1)
    char buf[64];
    snprintf(buf, sizeof(buf), "\tDSVF L%d", L1); emit(buf);
    
    // [Código do 'then']
    parse_statement();
    
    // DSVS L2: "Desvie Sempre". Pula o 'else' e vai pro FIM (L2)
    snprintf(buf, sizeof(buf), "\tDSVS L%d", L2); emit(buf);
    
    // L1: (aqui começa o 'else')
    emit_lab(L1);
    if (look.atomo == ELSE) {
        consome(ELSE);
        parse_statement(); // [Código do 'else']
    }
    
    // L2: (aqui é o 'fim-do-if')
    emit_lab(L2);
}

/* while_statement -> 'while' expression 'do' statement */
static void parse_while_statement(void) {
    consome(WHILE);
    
    // Pega dois rótulos: L1 (início do loop) e L2 (fim do loop)
    int L1 = proximo_rotulo();
    int L2 = proximo_rotulo();
    
    // L1: (aqui começa o loop)
    emit_lab(L1);
    
    // Gera código da condição
    parse_expression();
    
    // DSVF L2: "Desvie Se Falso". Se (condição) == 0, pula pro FIM (L2)
    char buf[64];
    snprintf(buf, sizeof(buf), "\tDSVF L%d", L2); emit(buf);
    
    consome(DO);
    // [Código do 'do']
    parse_statement();
    
    // DSVS L1: "Desvie Sempre". Volta pro INÍCIO (L1)
    snprintf(buf, sizeof(buf), "\tDSVS L%d", L1); emit(buf);
    
    // L2: (aqui é o 'fim-do-while')
    emit_lab(L2);
}

/* (As 3 funções a seguir cuidam da precedência de operadores) */
/* expression -> simple_expression [ (op_relacional) simple_expression ] */
static void parse_expression(void) {
    parse_simple_expression();
    if (is_relational_or_logic_op(look.atomo)) {
        TAtomo op = look.atomo;
        consome(look.atomo);
        parse_simple_expression();
        emit_compare_or_logic(op); // Gera CMIG, CMMA, etc.
    }
}

/* simple_expression -> term (('+'|'-'|'or'|'and') term)* */
static void parse_simple_expression(void) {
    parse_term();
    // (O enunciado simplifica e deixa misturar 'or'/'and' aqui)
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
            // ANÁLISE SEMÂNTICA: 'b' existe?
            int end = ts_busca_end(look.atributo.id);
            // CRVL: Carrega Valor (de variável)
            char buf[64]; snprintf(buf, sizeof(buf), "\tCRVL %d", end);
            emit(buf);
            consome(IDENTIFIER);
        } break;
        case CONSTINT:
            // CRCT: Carrega Constante
            emit1("\tCRCT %d", look.atributo.numero);
            consome(CONSTINT);
            break;
        case CONSTCHAR:
            emit1("\tCRCT %d", (int)look.atributo.ch); // empilha o código ASCII
            consome(CONSTCHAR);
            break;
        case TRUE:
            emit("\tCRCT 1"); consome(TRUE); break;
        case FALSE:
            emit("\tCRCT 0"); consome(FALSE); break;
        case ABRE_PAR:
            consome(ABRE_PAR);
            parse_expression(); // Trata a expressão dentro dos parênteses
            consome(FECHA_PAR);
            break;
        case NOT:
            consome(NOT);
            parse_factor();
            emit("\tNEGA"); // Nega o resultado (0->1 ou 1->0)
            break;
        default:
            erro_sintatico(IDENTIFIER);
    }
}

// Helper: é um operador relacional?
static int is_relational_or_logic_op(TAtomo t) {
    return t == IGUAL || t == DIFERENTE ||
           t == MENOR || t == MENOR_IGUAL ||
           t == MAIOR || t == MAIOR_IGUAL ||
           t == OR    || t == AND;
}

// Helper: emite o código MEPA para a comparação
static void emit_compare_or_logic(TAtomo op) {
    switch (op) {
        case IGUAL:       emit("\tCMIG"); break; // Compara Igual
        case DIFERENTE:   emit("\tCMDG"); break; // Compara Desigual
        case MENOR:       emit("\tCMME"); break; // Compara Menor
        case MENOR_IGUAL: emit("\tCMEG"); break; // Compara Menor ou Igual
        case MAIOR:       emit("\tCMMA"); break; // Compara Maior
        case MAIOR_IGUAL: emit("\tCMAG"); break; // Compara Maior ou Igual
        case OR:          emit("\tDISJ"); break; // Disjunção (OU)
        case AND:         emit("\tCONJ"); break; // Conjunção (E)
        default: break;
    }
}

/* ======================== 9. Main ======================== */

int main(int argc, char** argv) {
    if (argc < 2) {
        printf("\nUso: %s <arquivo_fonte>\n\n", argv[0]);
        return 1;
    }
    FILE* f = fopen(argv[1], "rb");
    if (!f) { perror("Erro ao abrir arquivo"); return 1; }

    // Lê o arquivo-fonte inteiro para um buffer na memória
    fseek(f, 0, SEEK_END);
    long tam = ftell(f);
    fseek(f, 0, SEEK_SET);

    // calloc zera a memória, garantindo o '\0' no final
    entrada = (char*)calloc((size_t)tam + 1, 1);
    if (!entrada) { fclose(f); fprintf(stderr, "Falha ao alocar memoria\n"); return 1; }
    fread(entrada, 1, (size_t)tam, f);
    fclose(f);

    // Prepara o compilador
    cursor = entrada;
    avancar_lex();       // Pega o primeiro token
    
    // Começa a mágica (Análise + Geração de Código)
    parse_program();

    // Se a compilação terminou e não estamos no fim (EOS), é um erro
    if (look.atomo != EOS) erro_sintatico(EOS);

    // Imprime a Tabela de Símbolos (requisito do trabalho)
    ts_print();

    // Limpa a sujeira (memória alocada)
    ts_free();
    free(entrada);
    return 0;
}