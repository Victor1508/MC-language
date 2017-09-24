/**
 * Student name: Nguyen Mau Vinh
 * Student ID: 1627058
 */
grammar MC;

@lexer::header{
  package mc.parser;
}

@lexer::members{
@Override
public Token emit() {
    switch (getType()) {
    case UNCLOSE_STRING:       
        Token result = super.emit();
        // you'll need to define this method
        throw new UncloseString(result.getText());
        
    case ILLEGAL_ESCAPE:
      result = super.emit();
      throw new IllegalEscape(result.getText());

    case ERROR_CHAR:
      result = super.emit();
      throw new ErrorToken(result.getText()); 

    default:
        return super.emit();
    }
}
}

@parser::header{
  package mc.parser;
}

options{
  language=Java;
}

program
    : decl+ EOF
    ;

decl
    : vardecl
    | fundecl
    ;

vardecl
    : primtype varname (COMMA varname)* SEMI
    ;

fundecl
    : rettype ID LB paralist? RB blockstmt
    ;

rettype
    : primtype
    | VOIDTYPE
    | arrptrtype
    ;

primtype
    : INTTYPE
    | FLOATTYPE
    | BOOLTYPE
    | STRINGTYPE
    ;

arrtype
    : primtype LS INTLIT RS
    ;

arrptrtype
    : primtype LS RS
    ;

varname
    : ID (LS INTLIT RS)?
    ;

paralist    
    : para (COMMA para)*
    ;

para
    : primtype ID (LS RS)?
    ;

blockstmt
    : LP vardecl* stmt* RP
    ;

stmt
    : matchstmt
    | unmatchstmt
    ;

matchstmt
    : IF LB exp RB matchstmt ELSE matchstmt
    | dowhilestmt
    | forstmt
    | brkstmt
    | contstmt
    | retstmt
    | expstmt
    | blockstmt
    ;

unmatchstmt
    : IF LB exp RB (stmt | matchstmt ELSE unmatchstmt)
    ;

dowhilestmt
    : DO stmt+ WHILE exp SEMI
    ;

forstmt
    : FOR LB exp SEMI exp SEMI exp RB stmt
    ;

brkstmt
    : BREAK SEMI
    ;

contstmt
    : CONTINUE SEMI
    ;

retstmt
    : RETURN exp? SEMI
    ;

expstmt
    : exp SEMI
    ;

exp
    : exp1 ASSIGN exp
    | exp1
    ;

exp1
    : exp1 OROP exp2
    | exp2
    ;

exp2
    : exp2 ANDOP exp3
    | exp3
    ;

exp3
    : exp4 EQUALOP exp4
    | exp4
    ;

exp4
    : exp5 COMPOP exp5
    | exp5
    ;

exp5
    : exp5 (ADDOP | SUBOP) exp6
    | exp6
    ;

exp6
    : exp6 (MULOP | DIVOP | MODOP) exp7
    | exp7
    ;

exp7
    : (ADDOP | SUBOP | NEGOP) exp7
    | exp8
    ;

exp8
    : LB exp RB
    | ID
    | literal
    | funcall
    | exp8 LS exp RS
    ;
    
literal
    : INTLIT
    | FLOATLIT
    | BOOLLIT
    | STRINGLIT
    ;

funcall
    : ID LB (exp (COMMA exp)*)? RB
    ;

BLOCK_COMMENT
    : '/*' .*? '*/' -> skip
    ;

LINE_COMMENT
    : '//' ~[\r\n]* -> skip
    ;

INTTYPE
    : 'int'
    ;

VOIDTYPE
    : 'void'
    ;

FLOATTYPE
    : 'float'
    ;

BOOLTYPE
    : 'boolean'
    ;

STRINGTYPE
    : 'string'
    ;

IF
    : 'if'
    ;

ELSE
    : 'else'
    ;

FOR
    : 'for'
    ;

DO
    : 'do'
    ;

WHILE
    : 'while'
    ;

BREAK
    : 'break'
    ;

CONTINUE
    : 'continue'
    ;

RETURN
    : 'return'
    ;

BOOLLIT
    : 'true'
    | 'false'
    ;

ID
    : [a-zA-Z_][a-zA-Z0-9_]*
    ;

INTLIT: [0-9]+;

FLOATLIT
    : [0-9]+'.'?[0-9]*([eE][+-]?[0-9]+)?
    | '.'[0-9]+([eE][+-]?[0-9]+)? 
    ;

STRINGLIT
    : '"'(ESC | ~[\r\n"\\EOF])*'"' 
        {
            String s = getText();
            s = s.substring(1, s.length() - 1);
            setText(s);
        }
    ;


fragment ESC
    : '\\'['"\\bfrnt]
    ;

ASSIGN
    : '='
    ;

OROP
    : '||'
    ;

ANDOP
    : '&&'
    ;

EQUALOP
    : '=='
    | '!='
    ;

COMPOP
    : '<'
    | '>'
    | '<='
    | '>='
    ;

ADDOP
    : '+'
    ;

SUBOP
    : '-'
    ;

MULOP
    : '*'
    ;

DIVOP
    : '/'
    ;

MODOP
    : '%'
    ;

NEGOP
    : '!'
    ;

LB
    : '('
    ;

RB
    : ')'
    ;

LP
    : '{'
    ;

RP
    : '}'
    ;

LS
    : '['
    ;

RS
    : ']'
    ;

SEMI
    : ';'
    ;

COLON
    : ':'
    ;

COMMA
    : ','
    ;

WS
    : [ \t\r\n]+ -> skip
    ;


ERROR_CHAR: .;

ILLEGAL_ESCAPE
    : '"'(ESC | ~[\r\n"\\EOF])*'\\'
      {
        if(true)
          throw new mc.parser.IllegalEscape(getText().substring(1, getText().length()));
      }
    ;

UNCLOSE_STRING
    : '"'(ESC | ~[\r\n"\\EOF])*
      {
        if(true)
          throw new mc.parser.UncloseString(getText().substring(1, getText().length()));
      }
    ;

