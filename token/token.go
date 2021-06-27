package token

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
}

const (
	ILLEGAL = "ILLEGAL"
	EOF     = "EOF"

	// 識別子，リテラル
	IDENT  = "IDENT"
	INT    = "INT"
	STRING = "STRING"

	// 演算子
	ASSIGN   = "="
	PLUS     = "+"
	MINUS    = "-"
	BANG     = "!"
	ASTERISK = "*"
	SLASH    = "/"
	MOD      = "%"

	INC        = "++"
	DEC        = "--"
	ASSIGN_ADD = "+="
	ASSIGN_SUB = "-="
	ASSIGN_MUL = "*="
	ASSIGN_DIV = "/="
	ASSIGN_MOD = "%="

	LT     = "<"
	GT     = ">"
	EQ     = "=="
	NOT_EQ = "!="

	// デリミタ
	COMMA     = ","
	SEMICOLON = ";"
	COLON     = ":"

	LPAREN   = "("
	RPAREN   = ")"
	LBRACE   = "{"
	RBRACE   = "}"
	LBRACKET = "["
	RBRACKET = "]"

	// キーワード
	FUNCTION = "FUNCTION"
	LET      = "LET"
	TRUE     = "TRUE"
	FALSE    = "FALSE"
	IF       = "IF"
	ELSE     = "ELSE"
	RETURN   = "RETURN"
	MACRO    = "MACRO"
	WHILE    = "WHILE"
	BREAK    = "BREAK"
)

var keywords = map[string]TokenType{
	"fn":     FUNCTION,
	"let":    LET,
	"true":   TRUE,
	"false":  FALSE,
	"if":     IF,
	"else":   ELSE,
	"return": RETURN,
	"macro":  MACRO,
	"while":  WHILE,
	"break":  BREAK,
}

func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}
