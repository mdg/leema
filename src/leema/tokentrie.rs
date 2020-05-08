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
///   keyword match
///   function match
///   emit, push/pop/replace
///
/// InlineTrie
///   .new_exact("(", ParenL, Push(InlineTrie))
///   .new_exact(")", ParenR, Pop)
///   .new_func(is_alpha, IdState)
///   .new_func(is_numeric, IntState)
///
/// IdState
///   .continue_func(is_alphanum)
///   .emit(Id)
///
/// IntState
///   .continue_exact(".", Replace(FloatFirst))
///   .continue_func(is_numeric)
///   .emit(Int)
///
/// FloatFirst
///   .continue_func(is_numeric, Push(FloatMore))
///   .emit(Invalid)
/// FloatMore
///   .continue_func(is_numeric)
///   .emit(Float)
///
/// 1. keyword?
/// 2. id?
/// 3. float
/// 4. int
/// 5. string
///

trait TokenIter
{
}

enum LexAction
{
    CompleteToken(Token),
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
