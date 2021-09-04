pub enum Error { 
    RuntimeError(String, String),
    DeclarationError(String, String)
}