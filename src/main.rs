#[derive(Debug)]
enum JsTokenType {
    Function,
    Identifier,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Comma,
    If,
    Else,
    For,
    While,
    Const,
    Var,
    Let,
    Continue,
    Break,
    Export,
    Import,
    From,
    Return,
    Plus,
    Minus,
    PlusPlus,
    MinusMinus,
    PlusEquals,
    MinusEquals,
    Equals,
    EqualsEquals,
    EqualsEqualsEquals,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    FatArrow,
    Colon,
    Semicolon,
    Dot,
    DotDotDot,
    OrOr,
    Or,
    And,
    AndAnd,
    OrEquals,
    AndEquals,
    Xor,
    XorEquals,
    Bang,
    ShiftRight,
    ShiftRightZeroFill,
    ShiftLeft,
    BitwiseNot,
    Unexpected
}

#[derive(Debug)]
struct Token<TTokenType> {
    token_type: TTokenType,
    first: u32,
    last: u32,
}

trait Lexer<TTokenType> {
    // ctor
    fn new(text: &'static str) -> Self;

    // Returns the next token or None if there is nothing left
    fn next(&mut self) -> Option<Token<TTokenType>>;
}

struct JsLexer<'a> {
    chars: std::str::Chars<'a>,
    prev: Option<char>,
    current: Option<char>,
    backtracking: bool,
    first: u32,
    last: u32,
}

fn is_identifier_start_char(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_identifier_char(ch: char) -> bool {
    ch.is_digit(10) || ch.is_alphabetic() || ch == '_'
}

impl<'a> JsLexer<'a> {
    fn emit(&mut self, token_type: JsTokenType) -> Option<Token<JsTokenType>> {
        let result = Some(Token { token_type: token_type, first: self.first, last: self.last - 1 });
        self.first = self.last;
        result
    }

    fn emit_back(&mut self, token_type: JsTokenType) -> Option<Token<JsTokenType>> {
        self.backtrack();
        self.emit(token_type)
    }

    fn forward(&mut self) -> Option<char> {
        self.last += 1;
        if self.backtracking {
            self.backtracking = false;
            self.current
        } else {
            self.prev = self.current;
            self.current = self.chars.next();
            self.current
        }
    }

    fn backtrack(&mut self) {
        self.last -= 1;
        self.backtracking = true;
    }

    fn tok_or_ident(&mut self, token: JsTokenType) -> Option<Token<JsTokenType>> {
        let mut grew = false;
        loop {
            match self.forward() {
                Some(ch) => {
                    if !is_identifier_char(ch) {
                        self.backtrack();
                        break;
                    };
                },
                None => {
                    break;
                },
            };
            grew = true;
        };

        if grew {
            self.emit(JsTokenType::Identifier)
        } else {
            self.emit(token)
        }
    }

    fn continue_ident(&mut self) -> Option<Token<JsTokenType>> {
        match self.current {
            Some(c) if is_identifier_char(c) => self.tok_or_ident(JsTokenType::Identifier),
            _ => {
                self.backtrack();
                self.emit(JsTokenType::Identifier)
            },
        }
    }
}

impl<'a> Lexer<JsTokenType> for JsLexer<'a> {
    fn new(text: &'static str) -> JsLexer {
        JsLexer { chars: text.chars(), prev: None, current: None, backtracking: false, first: 0, last: 0 }
    }

    fn next(&mut self) -> Option<Token<JsTokenType>> {
        loop {
            match self.forward() {
                // break
                Some('b') => match self.forward() {
                    Some('r') => match self.forward() {
                        Some('e') => match self.forward() {
                            Some('a') => match self.forward() {
                                Some('k') => break self.tok_or_ident(JsTokenType::Break),
                                _ => break self.continue_ident(),
                            },
                            _ => break self.continue_ident(),
                        },
                        _ => break self.continue_ident(),
                    },
                    _ => break self.continue_ident(),
                },

                // const / continue
                Some('c') => match self.forward() {
                    Some('o') => match self.forward() {
                        Some('n') => match self.forward() {
                            Some('s') => match self.forward() {
                                Some('t') => break self.tok_or_ident(JsTokenType::Const),
                                _ => break self.continue_ident(),
                            },
                            Some('t') => match self.forward() {
                                Some('i') => match self.forward() {
                                    Some('n') => match self.forward() {
                                        Some('u') => match self.forward() {
                                            Some('e') => break self.tok_or_ident(JsTokenType::Continue),
                                            _ => break self.continue_ident(),
                                        },
                                        _ => break self.continue_ident(),
                                    },
                                    _ => break self.continue_ident(),
                                },
                                _ => break self.continue_ident(),
                            },
                            _ => break self.continue_ident(),
                        },
                        _ => break self.continue_ident(),
                    },
                    _ => break self.continue_ident(),
                },

                // else / export
                Some('e') => match self.forward() {
                    Some('l') => match self.forward() {
                        Some('s') => match self.forward() {
                            Some('e') => break self.tok_or_ident(JsTokenType::Else),
                            _ => break self.continue_ident(),
                        },
                        _ => break self.continue_ident(),
                    },
                    Some('x') => match self.forward() {
                        Some('p') => match self.forward() {
                            Some('o') => match self.forward() {
                                Some('r') => match self.forward() {
                                    Some('t') => break self.tok_or_ident(JsTokenType::Export),
                                    _ => break self.continue_ident(),
                                },
                                _ => break self.continue_ident(),
                            },
                            _ => break self.continue_ident(),
                        },
                        _ => break self.continue_ident(),
                    },
                    _ => break self.continue_ident(),
                },

                // for / from / function
                Some('f') => match self.forward() {
                    Some('o') => match self.forward() {
                        Some('r') => break self.tok_or_ident(JsTokenType::For),
                        _ => break self.continue_ident(),
                    },
                    Some('r') => match self.forward() {
                        Some('o') => match self.forward() {
                            Some('m') => break self.tok_or_ident(JsTokenType::From),
                            _ => break self.continue_ident(),
                        },
                        _ => break self.continue_ident(),
                    },
                    Some('u') => match self.forward() {
                        Some('n') => match self.forward() {
                            Some('c') => match self.forward() {
                                Some('t') => match self.forward() {
                                    Some('i') => match self.forward() {
                                        Some('o') => match self.forward() {
                                            Some('n') => break self.tok_or_ident(JsTokenType::Function),
                                            _ => break self.continue_ident(),
                                        },
                                        _ => break self.continue_ident(),
                                    },
                                    _ => break self.continue_ident(),
                                },
                                _ => break self.continue_ident(),
                            },
                            _ => break self.continue_ident(),
                        },
                        _ => break self.continue_ident(),
                    },
                    _ => break self.continue_ident(),
                },

                // if / import
                Some('i') => match self.forward() {
                    Some('f') => break self.tok_or_ident(JsTokenType::If),
                    Some('m') => match self.forward() {
                        Some('p') => match self.forward() {
                            Some('o') => match self.forward() {
                                Some('r') => match self.forward() {
                                    Some('t') => break self.tok_or_ident(JsTokenType::Import),
                                    _ => break self.continue_ident(),
                                },
                                _ => break self.continue_ident(),
                            },
                            _ => break self.continue_ident(),
                        },
                        _ => break self.continue_ident(),
                    },
                    _ => break self.continue_ident(),
                },

                // let
                Some('l') => match self.forward() {
                    Some('e') => match self.forward() {
                        Some('t') => self.tok_or_ident(JsTokenType::Let),
                        _ => break self.continue_ident(),
                    },
                    _ => break self.continue_ident(),
                },

                // return
                Some('r') => match self.forward() {
                    Some('e') => match self.forward() {
                        Some('t') => match self.forward() {
                            Some('u') => match self.forward() {
                                Some('r') => match self.forward() {
                                    Some('n') => break self.tok_or_ident(JsTokenType::Return),
                                    _ => break self.continue_ident(),
                                },
                                _ => break self.continue_ident(),
                            },
                            _ => break self.continue_ident(),
                        },
                        _ => break self.continue_ident(),
                    },
                    _ => break self.continue_ident(),
                },

                // var
                Some('v') => match self.forward() {
                    Some('a') => match self.forward() {
                        Some('r') => self.tok_or_ident(JsTokenType::Var),
                        _ => break self.continue_ident(),
                    },
                    _ => break self.continue_ident(),
                },

                // while
                Some('w') => match self.forward() {
                    Some('h') => match self.forward() {
                        Some('i') => match self.forward() {
                            Some('l') => match self.forward() {
                                Some('e') => break self.tok_or_ident(JsTokenType::While),
                                _ => break self.continue_ident(),
                            },
                            _ => break self.continue_ident(),
                        },
                        _ => break self.continue_ident(),
                    },
                    _ => break self.continue_ident(),
                },

                // . / ...
                Some('.') => match self.forward() {
                    Some('.') => match self.forward() {
                        Some('.') => break self.emit(JsTokenType::DotDotDot),
                        _ => break self.emit_back(JsTokenType::Unexpected),
                    },
                    _ => break self.emit_back(JsTokenType::Dot),
                },

                // = / == / === / =>
                Some('=') => match self.forward() {
                    Some('=') => match self.forward() {
                        Some('=') => break self.emit(JsTokenType::EqualsEqualsEquals),
                        _ => break self.emit_back(JsTokenType::EqualsEquals),
                    },
                    Some('>') => break self.emit(JsTokenType::FatArrow),
                    _ => break self.emit_back(JsTokenType::Equals),
                },

                // + / ++ / +=
                Some('+') => match self.forward() {
                    Some('+') => break self.emit(JsTokenType::PlusPlus),
                    Some('=') => break self.emit(JsTokenType::PlusEquals),
                    _ => break self.emit_back(JsTokenType::Plus),
                },

                // - / -- / -=
                Some('-') => match self.forward() {
                    Some('-') => break self.emit(JsTokenType::MinusMinus),
                    Some('=') => break self.emit(JsTokenType::MinusEquals),
                    _ => break self.emit_back(JsTokenType::Minus),
                },

                // < / <= / <<
                Some('<') => match self.forward() {
                    Some('<') => break self.emit(JsTokenType::ShiftLeft),
                    Some('=') => break self.emit(JsTokenType::LessThanOrEqual),
                    _ => break self.emit_back(JsTokenType::LessThan),
                },

                // > / >= / >> / >>>
                Some('>') => match self.forward() {
                    Some('>') => match self.forward() {
                        Some('>') => self.emit(JsTokenType::ShiftRightZeroFill),
                        _ => break self.emit_back(JsTokenType::ShiftRight),
                    },
                    Some('=') => break self.emit(JsTokenType::GreaterThanOrEqual),
                    _ => break self.emit_back(JsTokenType::GreaterThan),
                },

                // | / || / |=
                Some('|') => match self.forward() {
                    Some('|') => break self.emit(JsTokenType::OrOr),
                    Some('=') => break self.emit(JsTokenType::OrEquals),
                    _ => break self.emit_back(JsTokenType::Or),
                },

                // & | && | &=
                Some('&') => match self.forward() {
                    Some('&') => break self.emit(JsTokenType::AndAnd),
                    Some('=') => break self.emit(JsTokenType::AndEquals),
                    _ => break self.emit_back(JsTokenType::And),
                },

                // ^ / ^=
                Some('^') => match self.forward() {
                    Some('=') => break self.emit(JsTokenType::XorEquals),
                    _ => break self.emit_back(JsTokenType::Xor),
                },

                // Single symbols
                Some('(') => break self.emit(JsTokenType::OpenParen),
                Some(')') => break self.emit(JsTokenType::CloseParen),
                Some('{') => break self.emit(JsTokenType::OpenBrace),
                Some('}') => break self.emit(JsTokenType::CloseBrace),
                Some('[') => break self.emit(JsTokenType::OpenBracket),
                Some(']') => break self.emit(JsTokenType::CloseBracket),
                Some(',') => break self.emit(JsTokenType::Comma),
                Some(';') => break self.emit(JsTokenType::Semicolon),
                Some('!') => break self.emit(JsTokenType::Bang),
                Some('~') => break self.emit(JsTokenType::BitwiseNot),
                Some(':') => break self.emit(JsTokenType::Colon),

                // whitespace
                Some(' ') | Some('\n') | Some('\r') | Some('\t') => {
                    self.first += 1;
                    continue;
                },

                // eof
                None => break None,

                // *
                Some(ch) => {
                    if is_identifier_start_char(ch) {
                        break self.tok_or_ident(JsTokenType::Identifier);
                    };
                    break self.emit(JsTokenType::Unexpected);
                },
            };
        }
    }
}

struct Program {
    statements: Vec<Statement>,
}

enum Statement {
    Import(ImportStatement),
    FunctionDefinition,
    VariableDefinition,
    Expression,
}

struct FunctionDefinition {
    name: str,
    parameters: Vec<FunctionParameter>,
    body: Vec<Statement>,
}

struct FunctionParameter {
    name: string,
}

struct VariableDefinition {
    def_type: VariableDefinitionType,
    name: str,
    value: Option<Expression>,
    exported: bool,
}

enum VariableDefinitionType {
    Var,
    Const,
    Let,
}

struct ImportStatement {
    items: Vec<ImportItem>,
    from: str,
}

struct ImportItem {
    thing: str,
    alias: str,
}

enum LiteralExpr {
    Number(str),
    Str(str),
}

enum Expression {
    Function(FunctionExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    MemberAccess(MemberAccessExpr),
    FunctionCall(FunctionCallExpr),
}

struct FunctionExpr {
    def: FunctionDefinition,
    is_fat_arrow: boolean,
}

struct BinaryExpr {
    a: Expression,
    b: Expression,
    op: BinaryOp,
}

struct UnaryExpr {
    a: Expression,
    op: UnaryOp,
}

struct MemberAccessExpr {
    a: Expression,
    b: str,
}

struct FunctionCallExpr {
    callee: Expression,
    parameters: Vec<FunctionValue>,
}

struct FunctionValue {
    value: Expression,
}

enum BinaryOp {
    Add,
    Subtract,
    Divide,
    Multiply,
    BitwiseAnd,
    BitwiseOr,
}

enum UnaryOp {
    Not,
    Negative,
    Positive,
}

struct JsParser {
    prev: Option<Token<JsTokenType>>,
}

impl JsParser {
    fn new() -> JsParser {
        JsParser { prev: None }
    }

    fn receive_all(&mut self, tokens: Vec<Token<JsTokenType>>) {

    }
}

fn main() {
    let mut lexer = JsLexer::new("
import { asdg }, zxcv from qwer

function test(foo, bar) {
  const foobar = hello();

  if (foo >= bar) {
    console.log(foo + bar)
  }

  return hello
}");
    let mut parser = JsParser::new();
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        println!("{:?}", token);
        tokens.push(token)
        //parser.receive(token);
    }

    parser.receive_all(tokens);
}

// #[cfg(test)]
// mod tests {
//     use super::*;
// 
//     fn lex_fn() {
//         let code = "
//             function test() {
//                 return 5
//             }";
//         let tokens = lex(code);
//         assert_eq!(["function", "test", "parens", "braces"])
//     }
// 
//     #[test]
//     fn five_is_five() {
//         assert_eq!(5, five());
//     }
// }

