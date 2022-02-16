#include "stdio.h"
#include "stdlib.h"
#include "stdint.h"
#include "assert.h"
#include "string.h"
#include <string_view>
#include <stdexcept>

inline constexpr size_t KB = (size_t)1024;
inline constexpr size_t MB = (size_t)1024 * 1024;
inline constexpr size_t GB = (size_t)1024 * 1024 * 1024;
inline constexpr size_t TB = (size_t)1024 * 1024 * 1024 * 1024;


////// macro_stuff.hpp
#define STRINGIFY(x) #x

//// Preprocessor stuff
#ifdef __GNUC__ // GCC 4.8+, Clang, Intel and other compilers compatible with GCC (-std=c++0x or above)
	#define _ASSUME(cond) if (!(cond)) __builtin_unreachable()
	#define _UNREACHABLE __builtin_unreachable()
	#define _FORCEINLINE __attribute__((always_inline)) inline
	#define _NOINLINE    __attribute__((noinline))

#elif defined(_MSC_VER) // MSVC
	#define _ASSUME(cond) __assume(cond)
	#define _UNREACHABLE  __assume(false)
	#define _FORCEINLINE  __forceinline
	#define _NOINLINE     __declspec(noinline)
#else
	#define _ASSUME(cond)
	#define _UNREACHABLE  
	#define _FORCEINLINE  
	#define _NOINLINE     
#endif


////// errors.hpp

/*
[[noreturn]] inline void _ERROR (const char* errtype, SourceRange const& src, const char* format, const std::format_args _Args) {
	throw CompilerExcept{{ errtype, src, std::vformat(format, _Args) }};
}

template <typename... Args>
[[noreturn]] inline void SYNTAX_ERROR (SourceRange const& src, const char* format, Args... args) {
	_ERROR("syntax error", src, format, std::make_format_args(args...));
}
template <typename... Args>
[[noreturn]] inline void ERROR (SourceRange const& src, const char* format, Args... args) {
	_ERROR("error", src, format, std::make_format_args(args...));
}*/

#define SYNTAX_ERROR(SRC, FORMAT) throw std::runtime_error(FORMAT)


/////// lexer.hpp

enum TokenType : uint8_t {
	T_EOF   =0,

	T_LITERAL_BOOL,
	T_LITERAL_INT,
	T_LITERAL_FLT,
	T_LITERAL_STR,
	
	T_IDENTIFIER,
	T_FUNC,
	T_STRUCT,

	T_IF,
	T_ELIF,
	T_ELSE,

	T_WHILE,      
	T_FOR,        
	T_DO,         

	T_RETURN,     
	T_BREAK,      
	T_CONTINUE,   
	T_GOTO,       

	//
	T_COLON         ,//=':',
	T_SEMICOLON     ,//=';',
	T_COMMA         ,//=',',
	
	T_PAREN_OPEN    ,//='(',
	T_PAREN_CLOSE   ,//=')',
	T_BLOCK_OPEN    ,//='{',
	T_BLOCK_CLOSE   ,//='}',
	T_INDEX_OPEN    ,//='[',
	T_INDEX_CLOSE   ,//=']',
	
	T_ADD           ,//='+',
	T_SUB           ,//='-',
	T_MUL           ,//='*',
	T_DIV           ,//='/',
	T_MOD           ,//='%',
	
	T_BIT_AND       ,//='&',
	T_BIT_OR        ,//='|',
	T_BIT_XOR       ,//='^',
	
	T_AND           ,//='&' + 32,
	T_OR            ,//='|' + 32,
	
	T_LESS          ,//='<',
	T_LESSEQ        ,//='<' + 32,
	T_GREATER       ,//='>',
	T_GREATEREQ     ,//='>' + 32,
	T_EQUALS        ,//='=' + 32,
	T_NOT_EQUALS    ,//='!' + 32,
	
	T_MEMBER        ,//='.',
	
	T_QUESTIONMARK  ,//='?',
	
	T_BIT_NOT       ,//='~',
	T_NOT           ,//='!',
	T_INC           ,//='+' + 64,
	T_DEC           ,//='-' + 64,
	
	T_ASSIGN        ,//='=' + 32,
	T_ADDEQ         ,//='+' + 32,
	T_SUBEQ         ,//='-' + 32,
	T_MULEQ         ,//='*' + 32,
	T_DIVEQ         ,//='/' + 32,
	T_MODEQ         ,//='%' + 32,
};

inline constexpr const char* TokenType_str[] = {
	"T_EOF",         
	
	"T_LITERAL_BOOL",
	"T_LITERAL_INT", 
	"T_LITERAL_FLT", 
	"T_LITERAL_STR", 
	
	"T_IDENTIFIER",  

	"T_FUNC",        
	"T_STRUCT",      
	
	"T_IF",          
	"T_ELIF",        
	"T_ELSE",        

	"T_WHILE",       
	"T_FOR",         
	"T_DO",          

	"T_RETURN",      
	"T_BREAK",       
	"T_CONTINUE",    
	"T_GOTO",        

	"T_COLON",       
	"T_SEMICOLON",   
	"T_COMMA",       

	"T_PAREN_OPEN",  
	"T_PAREN_CLOSE", 
	"T_BLOCK_OPEN",  
	"T_BLOCK_CLOSE", 
	"T_INDEX_OPEN",  
	"T_INDEX_CLOSE", 

	"T_ADD",         
	"T_SUB",         
	"T_MUL",         
	"T_DIV",         
	"T_MOD",         

	"T_BIT_AND",     
	"T_BIT_OR",      
	"T_BIT_XOR",     

	"T_AND",         
	"T_OR",          

	"T_LESS",        
	"T_LESSEQ",      
	"T_GREATER",     
	"T_GREATEREQ",   
	"T_EQUALS",      
	"T_NOT_EQUALS",  

	"T_MEMBER",      

	"T_QUESTIONMARK",

	"T_BIT_NOT",     
	"T_NOT",         
	"T_INC",         
	"T_DEC",         

	"T_ASSIGN",      
	"T_ADDEQ",       
	"T_SUBEQ",       
	"T_MULEQ",       
	"T_DIVEQ",       
	"T_MODEQ",       
};
inline constexpr const char* TokenType_char[] = {
	"<EOF>"       ,
              
	"literal_bool",
	"literal_int" ,
	"literal_flt" ,
	"literal_str" ,
              
	"identifier"  ,
              
	"func"        ,
	"struct"      ,
              
	"if"          ,
	"elif"        ,
	"else"        ,
              
	"while"       ,
	"for"         ,
	"do"          ,
              
	"return"      ,
	"break"       ,
	"continue"    ,
	"goto"        ,
              
	":"           ,
	";"           ,
	","           ,
              
	"("           ,
	")"           ,
	"{"           ,
	"}"           ,
	"["           ,
	"]"           ,
              
	"+"           ,
	"-"           ,
	"*"           ,
	"/"           ,
	"%"           ,
              
	"&"           ,
	"|"           ,
	"^"           ,
              
	"&&"          ,
	"||"          ,
              
	"<"           ,
	"<="          ,
	">"           ,
	">="          ,
	"=="          ,
	"!="          ,
              
	"."           ,
              
	"?"           ,
              
	"~"           ,
	"!"           ,
	"++"          , // x++ is more readable, but profiling.hpp  wants the actual token chars
	"--"          ,
              
	"="           ,
	"+="          ,
	"-="          ,
	"*="          ,
	"/="          ,
	"%="          ,
};


// use small ints in SourceRange since it's only for debug info/printing
// so we don't care about handling degenerate cases like 64k long tokens etc.
// use this to cut down on the size of this struct a little, since every Token and AST node inludes an instance of this
// use these saturate functions to avoid wrap around if tokens ever are actually that long (TODO: not tested yet)

inline size_t saturate16 (size_t x) { return x <= UINT32_MAX ? x : UINT32_MAX; }
inline size_t saturate32 (size_t x) { return x <= UINT16_MAX ? x : UINT16_MAX; }

struct SourceRange {
	// first char as ptr into source (allows me to see things in debugger)
	char const* start;

	// lineno of start character     (1-based to match common text editors)
	uint32_t    start_lineno;

	// char index of start character (0-based TODO: also 1-based?)
	uint16_t    start_charno;

	// lengh of string starting from start (saturated on overflow)
	uint16_t    length;

	// offset of source token relative to start (for binary operators etc.) to show up like ~~~~^~~~~
	uint16_t    arrow;

	std::string_view text () const {
		return std::string_view(start, (size_t)length);
	}

	static SourceRange after_tok (SourceRange& src) {
		SourceRange r;
		r.start  = src.start + src.length;
		r.start_lineno = src.start_lineno;
		r.start_charno = (uint16_t)saturate16(src.start_charno + src.length);
		r.length = 1;
		r.arrow = 0;
		return r;
	}

	static SourceRange range (SourceRange& a, SourceRange& b) {
		SourceRange r = a;
		r.length = (uint16_t)saturate16((size_t)(b.start - a.start) + b.length);
		r.arrow  = 0;
		return r;
	}
	static SourceRange range_with_arrow (SourceRange& a, SourceRange& arrow, SourceRange& b) {
		SourceRange r = a;
		r.length = (uint16_t)saturate16((size_t)(b.start - a.start) + b.length);
		r.arrow  = (uint16_t)saturate16((size_t)(arrow.start - a.start));
		return r;
	}
};




struct Token {
	TokenType    type;
	SourceRange  src;
};

constexpr size_t _sr_sz = sizeof(SourceRange);
constexpr size_t _tok_sz = sizeof(Token);

// This lexer uses fills a fixed size buffer of tokens and allows you to consume tokens with eat()
// and view a small windows of past and future tokens with operator[]
struct Lexer {
	const char* cur_char;
	const char* cur_line;
	size_t      cur_lineno;

	Token*      cur_tok;
	
	static inline constexpr int LOOKBACK = 1;  // how many future tokens are safe to read
	static inline constexpr int LOOKAHEAD = 2; // how many past   tokens are safe to read
	
	static inline constexpr int WINDOW_SIZE = LOOKBACK + LOOKAHEAD; // how many buffered tokens are valid to read at any one point
	static inline constexpr int KEEP_TOKENS = WINDOW_SIZE -1; // how many tokens are kept in refill_buf() when reaching the end of the buffer

	static inline constexpr int BUFSZ = 256; // if  sizeof(Token) * BUFSZ  fits into a cpu cache level that may improve perf 
	
	Token buf[BUFSZ];

	Lexer (const char* src) {
		cur_char = src;
		
		cur_line = cur_char;
		cur_lineno = 1;

		cur_tok = &buf[0];

		lex(&buf[0], buf+BUFSZ);
	}

	// our lookahead rage moves over the end of buf
	// copy visible window (LOOKBACK - LOOKAHEAD) from end of buffer to start of buffer
	// and fill rest of buffer with new tokens
	_NOINLINE void refill_buf () {
		Token* src = cur_tok - LOOKBACK;
		int count = KEEP_TOKENS; // -1 since that one token was outside the window (that's what triggered refill_buf)

		assert(src >= buf && src+KEEP_TOKENS <= buf+BUFSZ);

		memmove(buf, src, KEEP_TOKENS * sizeof(Token));

		cur_tok = buf + LOOKBACK;
		Token* new_toks = buf + KEEP_TOKENS;

		//_DBG_CLEAR(new_toks, _DBG_MAGIC_UNINIT, (buf+BUFSZ - new_toks) * sizeof(Token));

		lex(new_toks, buf+BUFSZ);
	}
	
	void set_source_range_start (SourceRange* r, char const* start) {
		r->start        = start;
		r->start_lineno = (uint32_t)saturate32(cur_lineno                );
		r->start_charno = (uint16_t)saturate16((size_t)(start - cur_line));
		r->arrow        = 0;
	}
	void set_source_range_len (SourceRange* r, ptrdiff_t len) {
		r->length       = (uint16_t)saturate16((size_t)len);
	}

	SourceRange get_source_range (char const* start, char const* end) {
		SourceRange r;
		set_source_range_start(&r, start);
		set_source_range_len(&r, end - start);
		return r;
	}
	
	/*
	// actually defer the parsing of literals to the parsing (rather than during lexing)
	// since this avoids having to store the Value struct for every single token
	void parse_lit_bool    (const char* start, const char* end, Value* out_val);
	void parse_lit_integer (const char* start, const char* end, Value* out_val);
	void parse_lit_double  (const char* start, const char* end, Value* out_val);
	void parse_lit_string  (const char* start, const char* end, Value* out_val);

	TypeClass parse_literal (TokenType type, const char* start, const char* end, Value* out_val);
	*/
	
	void lex (Token* first_tok, Token* end_tok);

	// consume a token, ie. move the visible window of tokens forward by one
	// occasionally refills the buffer of tokens by lexing more of the source code
	_FORCEINLINE void eat () {
		cur_tok++;

		if (cur_tok > buf+BUFSZ - LOOKAHEAD)
			refill_buf();
	}

	// peek at a token in the currently visible window of i in [-LOOKABACK, LOOKAHEAD)
	// i = -1 is the last eaten token (only if token was actually eaten, be careful)
	// i = 0 is the next to be eaten token, 1 is the one after that etc.
	// NOTE: could be called peek()
	_FORCEINLINE Token& operator[] (int i) {
		assert(i >= -LOOKBACK && i < LOOKAHEAD);
		assert(cur_tok + i >= buf && cur_tok + i < buf+BUFSZ);

		return cur_tok[i];
	}
};

////// lexer.cpp

constexpr inline bool is_decimal_c (char c) {
	return c >= '0' && c <= '9';
}
constexpr inline bool is_hex_c (char c) {
	return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
}

constexpr inline bool is_ident_start_c (char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}
constexpr inline bool is_ident_c (char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || (c >= '0' && c <= '9');
}
constexpr inline bool is_whitespace_c (char c) {
	return c == ' ' || c == '\t';
}
constexpr inline bool is_newline_c (char c) {
	return c == '\n' || c == '\r';
}

// ughh... no range-based switch-case...
#define IDENT_START_CASES \
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': \
	case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': \
	case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z': \
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': \
	case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': \
	case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z': \
	case '_':

#define NUMBER_START_CASES \
	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':

/*
void Lexer::parse_lit_bool    (const char* start, const char* end, Value* out_val) {
	if (start[0] == 'f') {
		assert(std::string_view(start, (size_t)(end - start)) == "false");
		out_val->i = 0;
	}
	else { // if (start[0] == 'f') 
		assert(std::string_view(start, (size_t)(end - start)) == "true");
		out_val->i = 1;
	}
}

void Lexer::parse_lit_integer (const char* start, const char* end, Value* out_val) {
	const char* cur = start;
	assert(*cur >= '0' || *cur <= '9');
	
	int64_t out = 0;
	while ((*cur >= '0' && *cur <= '9')) { //  || *cur == '_'
		//if (*cur != '_') {
			out *= 10;
			out += *cur - '0';
		//}
		cur++;
	}

	if (cur != end)
		SYNTAX_ERROR(get_source_range(start, start+1), "integer parse error");

	out_val->i = (int)out;
}

void Lexer::parse_lit_double  (const char* start, const char* end, Value* out_val) {
	char const* cur = start;
	double val = strtod(start, (char**)&cur); // need to cast away const for strtod api

	if (cur != end)
		SYNTAX_ERROR(get_source_range(start, start+1), "float parse error");
	
	out_val->f = val;
}

void Lexer::parse_lit_string  (const char* start, const char* end, Value* out_val) {
	assert(*start == '"');
	start++;
	assert(end > start);
	end--;
	assert(*end == '"');

	// resulting strings should be shorter than escaped strings
	size_t max_len = end - start + 1; // +1 to add null terminator

	char* result = g_allocator.alloc_array<char>(max_len);

	const char* in = start;
	char* out = result;

	while (in < end) {
		if (*in == '\\') {
			auto start = in++;
			switch (*in++) {
				case '0' : *out++ = '\0'; break;
				case 'n' : *out++ = '\n'; break;
				case 'r' : *out++ = '\r'; break;
				case '\\': *out++ = '\\'; break;
				case '"' : *out++ = '\"'; break;
				default:
					SYNTAX_ERROR(get_source_range(start, in), "invalid escape sequence in literal string");
			}
		} else {
			*out++ = *in++;
		}
	}
	*out++ = '\0';

	size_t real_len = out - result; // real length of generated string
	// don't bother to reallocate the strings just to save a few bytes not needed due to escape sequences
	// g_allocator does not support reallocation currently

	out_val->str = result;
}

TypeClass Lexer::parse_literal (TokenType type, const char* start, const char* end, Value* out_val) {
	switch (type) {
		case T_LITERAL_BOOL:
			parse_lit_bool(start, end, out_val);
			return TY_BOOL;

		case T_LITERAL_INT:
			parse_lit_integer(start, end, out_val);
			return TY_INT;

		case T_LITERAL_FLT:
			parse_lit_double(start, end, out_val);
			return TY_FLT;

		case T_LITERAL_STR:
			parse_lit_string(start, end, out_val);
			return TY_STR;

		INVALID_DEFAULT;
	}
	return (TypeClass)0;
}*/

struct _Keyword {
	std::string_view   str;
	TokenType tok;
};

//////// keyword_hash.hpp
/* C++ code produced by gperf version 3.0.1 */
/* Command-line: ./bin/gperf.exe -L C++ lang.txt  */
/* Computed positions: -k'1,3' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

bool keyword_hash_compare (const char* a, const char* b, size_t len) {
	const char* end = a + len;
	while (a < end) {
		if (*a++ != *b++) return false;
	}
	return true;
}

size_t keyword_hash (const char* str, size_t len) {
	static constexpr char asso_values[] = {
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 10,  0,
		 0,  0,  0, 15, 25,  5, 25, 25,  0, 25,
		15, 25, 25, 25,  0,  0,  5,  5, 25,  0,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
		25, 25, 25, 25, 25, 25
	};
	size_t hval = len;

	switch (hval) {
		default:
			hval += asso_values[(unsigned char)str[2]];
			/*FALLTHROUGH*/
		case 2:
		case 1:
			hval += asso_values[(unsigned char)str[0]];
			break;
	}
	return hval;
}

TokenType get_keyword (const char* str, size_t len) {
	//static const char * wordlist[] =
	//  {
	//    "", "",
	//    "do",
	//    "for",
	//    "else",
	//    "false",
	//    "struct",
	//    "if",
	//    "",
	//    "elif",
	//    "while",
	//    "return",
	//    "", "",
	//    "true",
	//    "break",
	//    "", "", "",
	//    "func",
	//    "", "", "",
	//    "continue",
	//    "goto"
	//  };

	static constexpr _Keyword wordlist[] = {
		{"" },
		{"" },
		{ "do"        , T_DO             },
		{ "for"       , T_FOR            },
		{ "else"      , T_ELSE           },
		{ "false"     , T_LITERAL_BOOL   },
		{ "struct"    , T_STRUCT         },
		{ "if"        , T_IF             },
		{"" },
		{ "elif"      , T_ELIF           },
		{ "while"     , T_WHILE          },
		{ "return"    , T_RETURN         },
		{"" },
		{"" },
		{ "true"      , T_LITERAL_BOOL   },
		{ "break"     , T_BREAK          },
		{"" },
		{"" },
		{"" },
		{ "func"      , T_FUNC           },
		{"" },
		{"" },
		{"" },
		{ "continue"  , T_CONTINUE       },
		{ "goto"      , T_GOTO           },
	};
	
	constexpr size_t TOTAL_KEYWORDS  = 14;
	constexpr size_t MIN_WORD_LENGTH = 2;
	constexpr size_t MAX_WORD_LENGTH = 8;
	constexpr size_t MIN_HASH_VALUE  = 2;
	constexpr size_t MAX_HASH_VALUE  = 24;
	/* maximum key range = 23, duplicates = 0 */

	if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH) {
		size_t key = keyword_hash(str, len);

		if (key <= MAX_HASH_VALUE && key >= 0) {
			auto& word = wordlist[key];

			if (word.str.size() == len && keyword_hash_compare(str, word.str.data(), len)) {
				return word.tok;
			}
		}
	}
	return T_IDENTIFIER;
}


void Lexer::lex (Token* first_tok, Token* end_tok) {
	const char* cur = cur_char; // copy into local to help compiler avoid reloading this during the loop

	Token* out_tok = first_tok;

	auto newline = [&] () {
		assert(*cur == '\n' || *cur == '\r');

		// newline found
		char c = *cur++;

		// this code should even handle files with inconsistent unix vs windows newlines reasonably
		// "\n" "\r" "\n\r" "\r\n" each count as one newline while "\n\n" "\r\r" count as two
		if ((*cur == '\n' || *cur == '\r') && c != *cur)
			cur++;

		cur_line = cur;
		cur_lineno++;
	};

	for (;;) {
		switch (*cur) {
			// skip whitespace
			case '\n': case '\r': {
				newline();
				continue;
			}
			case ' ': case '\t': {
				cur++;
				continue;
			}
			
			case '/': { // '//' or '/*'

				// "//" line comment begin, skip until newline or EOF
				if (cur[1] == '/') {
					cur+=2;

					while (!(*cur == '\n' || *cur == '\r') && *cur != '\0')
						cur++; // skip anything until newline or EOF

					continue;
				}
				// "/*" block comment begin, skip until end of block comment while keeping track of nested block comments
				else if (cur[1] == '*') {
					cur+=2;

					size_t depth = 1;
					while (depth > 0) {
						if (*cur == '\0') {
							// TODO: also add note about block comment open location to error
							SYNTAX_ERROR(get_source_range(cur, cur+1), "end of file in block comment");
						}
						else if (cur[0] == '/' && cur[1] == '*') {
							cur += 2; // skip "/*"
							depth++;
						}
						else if (cur[0] == '*' && cur[1] == '/') {
							cur += 2; // skip "*/"
							depth--;
						}
						else if (*cur == '\n' || *cur == '\r') {
							newline();
						}
						else {
							cur++;
						}
					}
					continue;
				}
			} break;

			case '*': { // '*/'
				if (cur[1] == '/') {
					SYNTAX_ERROR(get_source_range(cur, cur+2), "unexpected block comment close");
				}
			} break;
		}

		// non-whitespace character outside of comment -> start of a token

		if (out_tok >= end_tok) {
			cur_char = cur;
			break; // end lexing loop
		}
		
		const char* start = cur;

		Token& tok = *out_tok++;

		set_source_range_start(&tok.src, start);
		
		// tok.source.length = LEN: avoid range check by not calling set_source_range_len()
		#define SIMPLE_TOK(TYPE, LEN) {             \
			tok.type = TYPE;                        \
			tok.src.length = (uint16_t)LEN;         \
			cur += LEN;                             \
			continue;                               \
		}

		switch (*cur) {

			case '\0': {
				tok.type = T_EOF;
				tok.src.length = 1;

				// break would exit switch
				// and we really want the EOF case to be in the switch rather than a if before it, since this removes one conditional from every token lexing)
				goto L_exit; // end lexing loop
			}

			case '+':
				if (cur[1] == '=')      SIMPLE_TOK(T_ADDEQ, 2)
				else if (cur[1] == '+') SIMPLE_TOK(T_INC,   2)
				else                    SIMPLE_TOK(T_ADD,   1)

			case '-':
				if (cur[1] == '=')      SIMPLE_TOK(T_SUBEQ, 2)
				else if (cur[1] == '-') SIMPLE_TOK(T_DEC,   2)
				else                    SIMPLE_TOK(T_SUB,   1)

			case '*':
				if (cur[1] == '=') SIMPLE_TOK(T_MULEQ,      2)
				else               SIMPLE_TOK(T_MUL,        1)
			
			case '/':
				if (cur[1] == '=') SIMPLE_TOK(T_DIVEQ,      2)
				else               SIMPLE_TOK(T_DIV,        1)
			
			case '%':
				if (cur[1] == '=') SIMPLE_TOK(T_MODEQ,      2)
				else               SIMPLE_TOK(T_MOD,        1)
			
			case '&':
				if (cur[1] == '&') SIMPLE_TOK(T_AND,        2)
				else               SIMPLE_TOK(T_BIT_AND,    1)
			
			case '|':
				if (cur[1] == '|') SIMPLE_TOK(T_OR,         2)
				else               SIMPLE_TOK(T_BIT_OR,     1)
			
			case '<':
				if (cur[1] == '=') SIMPLE_TOK(T_LESSEQ,     2)
				else               SIMPLE_TOK(T_LESS,       1)
			
			case '>':
				if (cur[1] == '=') SIMPLE_TOK(T_GREATEREQ,  2)
				else               SIMPLE_TOK(T_GREATER,    1)
			
			case '!':
				if (cur[1] == '=') SIMPLE_TOK(T_NOT_EQUALS, 2)
				else               SIMPLE_TOK(T_NOT,        1)
			
			case '=':
				if (cur[1] == '=') SIMPLE_TOK(T_EQUALS,     2)
				else               SIMPLE_TOK(T_ASSIGN,     1)
				
			case '~':      SIMPLE_TOK(T_BIT_NOT,         1)
			case '^':      SIMPLE_TOK(T_BIT_XOR,         1)
			
			case '.':      SIMPLE_TOK(T_MEMBER,          1)
			case ':':      SIMPLE_TOK(T_COLON,           1)
			case ';':      SIMPLE_TOK(T_SEMICOLON,       1)
			case ',':      SIMPLE_TOK(T_COMMA,           1)
			case '?':      SIMPLE_TOK(T_QUESTIONMARK,    1)
			
			case '(':      SIMPLE_TOK(T_PAREN_OPEN,      1)
			case ')':      SIMPLE_TOK(T_PAREN_CLOSE,     1)
			case '{':      SIMPLE_TOK(T_BLOCK_OPEN,      1)
			case '}':      SIMPLE_TOK(T_BLOCK_CLOSE,     1)
			case '[':      SIMPLE_TOK(T_INDEX_OPEN,      1)
			case ']':      SIMPLE_TOK(T_INDEX_CLOSE,     1)

			IDENT_START_CASES {
				while (is_ident_c(*cur))
					cur++; // find end of identifier

				set_source_range_len(&tok.src, cur - start);

				tok.type = get_keyword(start, (size_t)(cur - start));
				continue;
			}

			NUMBER_START_CASES {

				while (is_decimal_c(*cur))
					cur++;

				// float
				if (*cur == '.') {

					cur++;
					while (is_decimal_c(*cur))
						cur++;

					tok.type = T_LITERAL_FLT;
				}
				// int
				else {
					tok.type = T_LITERAL_INT;
				}
				set_source_range_len(&tok.src, cur - start);
				continue;
			}

			case '"': {
				cur++; // skip '"'

				for (;;) {
					if (*cur == '\0') {
						SYNTAX_ERROR(get_source_range(cur, cur+1), "end of file in string literal");
					}
					else if (*cur == '\n' || *cur == '\r') {
						//SYNTAX_ERROR(get_source_range(cur, cur+1), "newline in string literal"); // Allow newlines?
						newline();
					}
					// escape sequences \\ and \"
					else if (cur[0] == '\\' && (cur[1] == '"' || cur[1] == '\\')) {
						cur += 2;
					}
					else if (*cur == '"') {
						break;
					}
					cur++;
				}

				cur++; // skip '"'

				tok.type = T_LITERAL_STR;
				set_source_range_len(&tok.src, cur - start);
				continue;
			}

			default: {
				SYNTAX_ERROR(tok.src, "unknown token");
			}
		}
	}
	L_exit:

	cur_char = cur;
}

#undef SIMPLE_TOK

////// main.cpp

uint64_t get_file_size (FILE* f) {
    fseek(f, 0, SEEK_END);
    uint64_t file_size = ftell(f); // only 32 support for now
    rewind(f);
    return file_size;
}

// reads text file into a std::string (overwriting it's previous contents)
// returns false on fail (file not found etc.)
bool load_text_file (const char* filename, std::string* out) {
    FILE* f = fopen(filename, "rb"); // read binary because i don't want to convert "\r\n" to "\n"
    if (!f)
        return false; // fail

    uint64_t file_size = get_file_size(f);

    out->resize(file_size);

    auto ret = fread(&(*out)[0], 1,file_size, f);
    assert(ret == file_size);

    fclose(f);
    return true;
}

// reads text file into a std::string
// returns "" on fail (file not found etc.)
std::string load_text_file (const char* filename) {
    std::string s;
    load_text_file(filename, &s);
    return s;
}

_NOINLINE void profile_lexer (char const* filename) {
	std::string file = load_text_file(filename);
	
    // <TIMER> need to plug in your own timer here possibly std::chrono
    // (I use QueryPerformanceCounter for high-perf measuring on windows)

	//auto timer = Timer::start();
	{
		Lexer lex{file.c_str()};

		while (lex[0].type != T_EOF) {
			lex.eat();
		}
	}
	//float time = timer.end();

	//printf("%30s: %8llu MB  in  %6.2f ms  ->  %6.2f MB/s\n", filename,
	//	file.size() / MB,
	//	time * 1000,
	//	(float)file.size() / time / (float)MB);
}

int main () {
	printf("profiling files...\n");
	
	profile_lexer("prof_lex0_simple_toks.la");
	profile_lexer("prof_lex1_whitespace_comm.la");
	profile_lexer("prof_lex2_whitespace.la");
	profile_lexer("prof_lex3_numbers.la");
	profile_lexer("prof_lex4_identifiers.la");
	profile_lexer("test_100k.la");
	profile_lexer("prof_lex5_realcode.la");
	
	return 0;
}
