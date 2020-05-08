/// Lexer
///
/// newline state
///   emit push indent state
///   emit empty line
///
/// indent state
///   emit indent replace inline
///
/// inline state
///   keyword trie state
///   id iter
///   float iter
///   int iter
///   " emit and push string state
///   ( emit and push inline state
///   ) emit and pop inline state
///   [ emit and push inline state
///   ] emit and pop inline state
///   newline emit and pop state
///
/// string state
///   " emit and pop state
///   $ push string expr state
///
/// string expr state
///   ( emit and push inline state
///   id iter
///
/// regex state
///   probably like strings
///
/// line comment state
///   until newline
/// block comment state
///   munch munch munch
///
/// TokenIter
///   CharIter
///
/// TrieState
///   exact/function/nomatch
///   emit/begin/continue
///   push/pop/replace

/// NewlineTrie
///   .exact_open("\t", Tabs)
///   .exact_open(" ", Spaces)
///   .exact_emit("\n", EmptyLine)
///   .nomatch_emit(anything_else, Indent, InlineTrie)
///
/// InlineTrie
///   .exact_push("(", ParenL, InlineTrie)
///   .exact_pop(")", ParenR, Pop)
///   .exact_emit("-", Dash)
///   .exact_emit("--", DoubleDash)
///   .exact_close("\n", EOL)
///   .exact_close("\t", Invalid)
///   .func_open(is_alpha, IdState)
///   .func_open(is_numeric, IntState)
///   .func_open(is_space, Whitespace)
///
/// IdState
///   .continue_func(is_alphanum)
///   .nomatch_emit(Id)
///
/// IntState
///   .continue_exact(".", Replace(FloatFirst))
///   .continue_func(is_numeric)
///   .nomatch_emit(Int)
///
/// FloatFirst
///   .continue_func(is_numeric, Push(FloatMore))
///   .nomatch_emit(Invalid)
/// FloatMore
///   .continue_func(is_numeric)
///   .emit(Float)

/// 1. keyword?
/// 2. id?
/// 3. float
/// 4. int
/// 5. string
///

struct TokenIter
{
    chars: CharIter,
    text: &'static str,
    start: Char,
}

struct TrieIter
{
    parent: &TrieIter,
    ts: &TrieState,
    chars: TokenIter,
}

impl TrieIter
{
    pub fn step(&mut self)
    {
        let c = self.chars.peek();
        let mut new_start = false;
        let result = self.ts.check(c);
        if result.is_match() {
            self.chars.next();
        } else if let Some(tok) = result.emission() {
            token = self.chars.token(tok);
            new_start = true;
        }
        match result.staction() {
            Push(newstate) => {
                TrieIter{
                    parent: self,
                    ts: newstate,
                    chars: if new_start {
                        self.chars.new_start()
                    } else {
                        self.chars.clone()
                    }
                }
            }
            Replace(newstate) => {
                TrieIter{
                    parent: self.parent,
                    ts: newstate,
                    chars: self.chars.clone(),
                }
            }
            Pop => self.parent,
        }
    }
}

struct TrieState
{
    trie: Trie<Char, LexAction>,
    fmatch: Vec<MatchF>,
    nomatch: LexAction,
}

enum TokenAction
{
    Emit(Token),
    Emit(Token),
    Continue,
    PartialToken(Token),
    Invalid,
    PushState(TokenMode),
    PopState,
}

enum TokenRule
{
    Valid(Token),
    ValidWhile(Token),
    Invalid,
    PushTree(TokenMode),
    PopTree,
}

struct TokenTrie
{
    next: Vec<Char>,
    rule: Option<Token>,
}

impl TokenTrie
{
    pub fn insert_keyword(&mut self, kw &str)
    {
    }
}

impl Matcher
{
    pub fn push(&mut self, c: Char) -> Option<Token>
    {
        None
    }

    pub fn close(&mut self) -> Option<Token>
    {
    }
}

impl Lexer
{
    pub fn push(&mut self, c: Char) -> Option<Token>
    {
        None
    }

    pub fn close(&mut self) -> Option<Token>
    {
    }
}
