mod ast;
mod parser;

pub use ast::*;
pub use parser::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(u32);

impl From<&str> for Symbol {
    fn from(_value: &str) -> Self {
        unimplemented!()
    }
}
impl From<&String> for Symbol {
    fn from(value: &String) -> Self {
        value.as_str().into()
    }
}
impl From<String> for Symbol {
    fn from(value: String) -> Self {
        value.as_str().into()
    }
}
impl From<Symbol> for &'static str {
    fn from(_value: Symbol) -> Self {
        unimplemented!()
    }
}
impl std::fmt::Display for Symbol {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unimplemented!()
    }
}

pub type IndexMap<K, V> = std::collections::HashMap<K, V>;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct OrderedFloat<T>(pub T);

impl Eq for OrderedFloat<f64> {}
impl Ord for OrderedFloat<f64> {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        unimplemented!()
    }
}
impl std::hash::Hash for OrderedFloat<f64> {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        unimplemented!()
    }
}

fn main() {
    println!("Hello, world!");
}
